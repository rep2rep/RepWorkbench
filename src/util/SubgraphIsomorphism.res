@new external mkSize: int => array<'a> = "Array"

module Matrix: {
  type t
  let create: (~rows: int, ~columns: int) => t
  let copy: t => t
  let size: t => (int, int)
  let set: (t, int, int, int) => bool
  let permuteColumns: (t, array<int>) => unit
  let check: (t, int, int, int) => bool
} = {
  type t = array<array<int>>

  let create = (~rows, ~columns) =>
    mkSize(rows)->Array.map(_ => mkSize(columns)->Js.Array2.fillInPlace(0))
  let size = t => (Array.length(t), Array.length(t[0]->Option.getWithDefault([])))

  let get = (t, m, n) => t[m]->Option.flatMap(row => row[n])
  let set = (t, m, n, v) =>
    t[m]->Option.map(row => row->Array.set(n, v))->Option.getWithDefault(false)

  let permuteArray = (arr, perm) => {
    let arr' = Array.copy(arr)
    perm->Array.forEachWithIndex((new_, old_) => {
      let new_row = arr'->Array.getExn(new_)
      arr->Array.set(old_, new_row)->ignore
    })
  }
  let permuteColumns = (t, perm) => t->Array.forEach(row => permuteArray(row, perm))
  let copy = t => t->Array.map(r => r->Array.copy)
  let check = (t, m, n, v) => t->get(m, n)->Option.map(v' => v === v')->Option.getWithDefault(false)
}

type graph<'node, 'link> = (Gid.Map.t<'node>, array<(Gid.t, Gid.t, 'link)>)

// Convert the "isomorphism" into an actual subgraph we can use
let convertIsomorphism = (
  iso,
  whole,
  find,
  equiv_schemas,
  equiv_links,
  order_schemas_source,
  order_schemas_target,
) => {
  let mapping = []
  let (m, n) = Matrix.size(iso)
  Belt.Range.forEach(0, m - 1, row =>
    Belt.Range.forEach(0, n - 1, col => {
      if iso->Matrix.check(row, col, 1) {
        let id_in_whole = order_schemas_source[col]->Option.getExn
        let id_in_find = order_schemas_target[row]->Option.getExn
        mapping->Js.Array2.push((id_in_find, id_in_whole))->ignore
      }
    })
  )
  let (whole_schemas, whole_links) = whole
  let (find_schemas, find_links) = find
  // Determine if the schemas are compatible
  let schemas_ok = mapping->Array.every(((find_id, whole_id)) =>
    find_schemas
    ->Gid.Map.get(find_id)
    ->Option.flatMap(find_s =>
      whole_schemas
      ->Gid.Map.get(whole_id)
      ->Option.map(whole_s => {
        equiv_schemas(whole_s, find_s)
      })
    )
    ->Option.getWithDefault(false)
  )
  if schemas_ok {
    // Schemas are OK! Now check the links
    let mapping' = Gid.Map.fromArray(mapping)
    let links_ok = find_links->Array.every(((find_src, find_tgt, find_kind)) => {
      whole_links->Array.some(((whole_src, whole_tgt, whole_kind)) =>
        equiv_links(whole_kind, find_kind) &&
        mapping'
        ->Gid.Map.get(find_src)
        ->Option.map(id => id === whole_src)
        ->Option.getWithDefault(false) &&
        mapping'
        ->Gid.Map.get(find_tgt)
        ->Option.map(id => id === whole_tgt)
        ->Option.getWithDefault(false)
      )
    })
    if links_ok {
      // Hooray! We have an isomorphism!
      // Let's build the actual sub-model that matches it.
      let schemas =
        mapping
        ->Array.mapPartial(((_, sch_id)) =>
          whole_schemas->Gid.Map.get(sch_id)->Option.map(sch => (sch_id, sch))
        )
        ->Gid.Map.fromArray
      let links = find_links->Array.mapPartial(((find_src, find_tgt, find_kind)) => {
        whole_links->Array.find(((whole_src, whole_tgt, whole_kind)) =>
          equiv_links(whole_kind, find_kind) &&
          mapping'
          ->Gid.Map.get(find_src)
          ->Option.map(id => id === whole_src)
          ->Option.getWithDefault(false) &&
          mapping'
          ->Gid.Map.get(find_tgt)
          ->Option.map(id => id === whole_tgt)
          ->Option.getWithDefault(false)
        )
      })
      Some((schemas, links))
    } else {
      None
    }
  } else {
    None
  }
}

// Ullman's thing is complicated, let's do it an easier way.
let rec backtrack = (~next, ~isGoal, ~apply, state) => {
  if isGoal(state) {
    apply(state)
  } else {
    next(state)->Array.forEach(s => backtrack(~next, ~isGoal, ~apply, s))
  }
}

let findIsomorphism = (
  ~whole as (source_schemas, source_links),
  ~find as (target_schemas, target_links),
  ~equiv_schemas,
  ~equiv_links,
) => {
  let n_schemas_source = Gid.Map.size(source_schemas)
  let n_schemas_target = Gid.Map.size(target_schemas)
  let order_schemas_source = Gid.Map.keys(source_schemas) // Mapping from [0, n_schemas_source) => Gids
  let order_schemas_target = Gid.Map.keys(target_schemas) // Ditto

  let m = Matrix.create(~rows=n_schemas_target, ~columns=n_schemas_source)
  Belt.Range.forEach(0, Int.min(n_schemas_source, n_schemas_target) - 1, i =>
    m->Matrix.set(i, i, 1)->ignore
  )

  let result = []
  let init = []
  let conv = iso =>
    convertIsomorphism(
      iso,
      (source_schemas, source_links),
      (target_schemas, target_links),
      equiv_schemas,
      equiv_links,
      order_schemas_source,
      order_schemas_target,
    )
  let apply = perm => {
    let m' = Matrix.copy(m)
    m'->Matrix.permuteColumns(perm)
    m'->conv->Option.iter(m => result->Js.Array2.push(m)->ignore)
  }
  let isGoal = perm => Array.length(perm) === n_schemas_source
  let next = perm => {
    let v = Array.length(perm)
    Array.range(0, v)->Array.map(j => {
      let (before, after) = Array.splitAt(perm, j)
      Array.concatMany([before, [v], after])
    })
  }
  backtrack(~next, ~isGoal, ~apply, init)
  result
}
