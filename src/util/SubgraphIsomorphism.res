@new external mkSize: int => array<'a> = "Array"

module Matrix: {
  type t
  let create: (~rows: int, ~columns: int) => t
  let copy: t => t
  let size: t => (int, int)
  let set: (t, int, int, int) => bool
  let check: (t, int, int, int) => bool
  let dropColumns: (t, Belt.Set.Int.t) => t
  let hasZeroRow: t => bool
} = {
  type t = array<array<int>>

  let create = (~rows, ~columns) =>
    mkSize(rows)->Array.map(_ => mkSize(columns)->Js.Array2.fillInPlace(0))
  let size = t => (Array.length(t), Array.length(t[0]->Option.getWithDefault([])))

  let get = (t, m, n) => t[m]->Option.flatMap(row => row[n])
  let set = (t, m, n, v) =>
    t[m]->Option.map(row => row->Array.set(n, v))->Option.getWithDefault(false)
  let copy = t => t->Array.map(r => r->Array.copy)
  let check = (t, m, n, v) => t->get(m, n)->Option.map(v' => v === v')->Option.getWithDefault(false)

  let dropColumns = (t, cols) =>
    t->Array.map(row => row->Array.keepWithIndex((_, idx) => !(cols->Belt.Set.Int.has(idx))))
  let hasZeroRow = t => t->Array.some(row => row->Array.every(v => v === 0))
}

type graph<'node, 'link> = (Gid.Map.t<'node>, array<(Gid.t, Gid.t, 'link)>)

let zero_columns = mat => {
  let (m, n) = Matrix.size(mat)
  let bad_cols = []
  Belt.Range.forEach(0, n - 1, col => {
    if Belt.Range.every(0, m - 1, row => mat->Matrix.check(row, col, 0)) {
      bad_cols->Js.Array2.push(col)->ignore
    }
  })
  bad_cols->Belt.Set.Int.fromArray
}

// Convert the "isomorphism" into an actual subgraph we can use
let convertIsomorphism = (
  iso,
  whole,
  find,
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
  let (_, find_links) = find
  // Determine if the schemas are compatible
  // Schemas are OK by construction! Now check the links
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
}

// Inspired by Ullman's algorithm
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
  ~onFind,
) => {
  let n_schemas_source = Gid.Map.size(source_schemas)
  let n_schemas_target = Gid.Map.size(target_schemas)
  let order_schemas_source = Gid.Map.keys(source_schemas) // Mapping from [0, n_schemas_source) => Gids
  let order_schemas_target = Gid.Map.keys(target_schemas) // Ditto

  let m = Matrix.create(~rows=n_schemas_target, ~columns=n_schemas_source)
  let getidxSrc = i =>
    order_schemas_source[i]->Option.flatMap(id => source_schemas->Gid.Map.get(id))
  let getidxTgt = i =>
    order_schemas_target[i]->Option.flatMap(id => target_schemas->Gid.Map.get(id))
  Belt.Range.forEach(0, n_schemas_target - 1, row =>
    Belt.Range.forEach(0, n_schemas_source - 1, col => {
      let equiv =
        (getidxSrc(col), getidxTgt(row))
        ->Option.both
        ->Option.map(((a, b)) => equiv_schemas(a, b))
        ->Option.getWithDefault(false)
      if equiv {
        m->Matrix.set(row, col, 1)->ignore
      }
    })
  )
  // We now remove all rows and columns that are "zeroed out" to reduce the search space.
  let zero_cols = zero_columns(m)
  let m = m->Matrix.dropColumns(zero_cols)
  let n_schemas_source = n_schemas_source - Belt.Set.Int.size(zero_cols)
  let order_schemas_source =
    order_schemas_source->Array.keepWithIndex((_, idx) => !(zero_cols->Belt.Set.Int.has(idx)))
  // Now the matrix is filled with all possible isomorphic pairs.
  // We need to make versions which contain at most one 1 in each column,
  // and at exactly one 1 in each row.
  // If there are any rows with all zeros, this is impossible.
  if !Matrix.hasZeroRow(m) {
    // available-points, row-to-modify, used-columns, columns-to-schemas, column-count
    let used = Gid.Set.empty
    let init = (m, 0, used, order_schemas_source, n_schemas_source)
    let conv = (iso, order_schemas_source) =>
      convertIsomorphism(
        iso,
        (source_schemas, source_links),
        (target_schemas, target_links),
        equiv_links,
        order_schemas_source,
        order_schemas_target,
      )
    let apply = ((m_sln, _, _, order_schemas_source, _)) => {
      conv(m_sln, order_schemas_source)->Option.iter(onFind)
    }
    let isGoal = ((_, d, _, _, _)) => d === n_schemas_target
    let next = ((m', d, used, order_schemas_source, n_schemas_source)) => {
      let n = []
      Belt.Range.forEach(0, n_schemas_source - 1, col => {
        // Check that we can still set this column...
        let is_used =
          order_schemas_source[col]
          ->Option.map(col => used->Gid.Set.has(col))
          ->Option.getWithDefault(true)
        if !is_used {
          // If we can, leave it one and set all others to zero
          let m'' = Matrix.copy(m')
          Belt.Range.forEach(0, n_schemas_source - 1, col' => {
            if col !== col' {
              m''->Matrix.set(d, col', 0)->ignore
            }
          })
          let used' =
            order_schemas_source[col]
            ->Option.map(id => used->Gid.Set.add(id))
            ->Option.getWithDefault(used)
          // If the row was zero'd out, then we're done! Otherwise, continue the search
          if !Matrix.hasZeroRow(m'') {
            // Again, remove all rows and columns that are "zeroed out" to reduce the search space.
            let zero_cols = zero_columns(m'')
            let m'' = m''->Matrix.dropColumns(zero_cols)
            let n_schemas_source = n_schemas_source - Belt.Set.Int.size(zero_cols)
            let order_schemas_source =
              order_schemas_source->Array.keepWithIndex((_, idx) =>
                !(zero_cols->Belt.Set.Int.has(idx))
              )
            n->Js.Array2.push((m'', d + 1, used', order_schemas_source, n_schemas_source))->ignore
          }
        }
      })
      n
    }
    backtrack(~next, ~isGoal, ~apply, init)
  }
}
