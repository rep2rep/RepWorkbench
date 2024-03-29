type nodes = Gid.Map.t<InspectorState.Schema.t>
type links = array<(Gid.t, Gid.t, ModelLink.Kind.t)>

let indent = "  "

let countNodeTypes = slots => {
  let inc = v => v->Option.map(x => x + 1)->Option.getWithDefault(1)->Some
  let counts =
    slots
    ->Gid.Map.values
    ->Array.reduce(String.Map.empty, (counts, s) =>
      switch s {
      | InspectorState.Schema.Representation(_) => counts->String.Map.update("Representation", inc)
      | InspectorState.Schema.Scheme(_) => counts->String.Map.update("R-Scheme", inc)
      | InspectorState.Schema.Dimension(_) => counts->String.Map.update("R-Dimension", inc)
      | InspectorState.Schema.Token(t) => {
          let counts = counts->String.Map.update("R-Symbol", inc)
          switch t.is_class {
          | None => counts
          | Some(false) => counts->String.Map.update(indent ++ "Single", inc)
          | Some(true) => counts->String.Map.update(indent ++ "Class", inc)
          }
        }
      | InspectorState.Schema.Placeholder(_) => counts->String.Map.update("Placeholder", inc)
      }
    )
  let keys = [
    "Representation",
    "R-Scheme",
    "R-Dimension",
    "R-Symbol",
    indent ++ "Single",
    indent ++ "Class",
    "Placeholder",
  ]
  let counts = keys->Array.keepMap(k => counts->String.Map.get(k)->Option.map(v => (k, v)))
  Array.concat(
    [("Schemas", slots->Gid.Map.size->Int.toString)],
    counts->Array.map(((k, v)) => (indent ++ k, Int.toString(v))),
  )
}

let countLinkTypes = links => {
  let inc = v => v->Option.map(x => x + 1)->Option.getWithDefault(1)->Some
  let counts = links->Array.reduce(String.Map.empty, (counts, (_, _, k)) =>
    switch k {
    | ModelLink.Kind.Hierarchy => counts->String.Map.update("Hierarchy", inc)
    | ModelLink.Kind.Anchor => counts->String.Map.update("Anchor", inc)
    | ModelLink.Kind.Generic => counts->String.Map.update("Generic", inc)
    }
  )
  let keys = ["Hierarchy", "Anchor", "Generic"]
  let counts = keys->Array.keepMap(k => counts->String.Map.get(k)->Option.map(v => (k, v)))
  Array.concat(
    [("Connections", links->Array.length->Int.toString)],
    counts->Array.map(((k, v)) => (indent ++ k, Int.toString(v))),
  )
}

let countQuantityScales = schemas => {
  let incConc = v => v->Option.map(((x, y)) => (x + 1, y))->Option.getWithDefault((1, 0))->Some
  let incGrap = v => v->Option.map(((x, y)) => (x, y + 1))->Option.getWithDefault((0, 1))->Some
  let scaleInc = (counts, scale, f) =>
    switch scale {
    | None => counts
    | Some(Quantity_scale.Nominal) => counts->String.Map.update("Nominal", f)
    | Some(Quantity_scale.Ordinal) => counts->String.Map.update("Ordinal", f)
    | Some(Quantity_scale.Interval) => counts->String.Map.update("Interval", f)
    | Some(Quantity_scale.Ratio) => counts->String.Map.update("Ratio", f)
    }
  let counts =
    schemas
    ->Gid.Map.values
    ->Array.reduce(String.Map.empty, (counts, s) =>
      switch s {
      | InspectorState.Schema.Dimension(d) =>
        counts->scaleInc(d.concept_scale, incConc)->scaleInc(d.graphic_scale, incGrap)
      | _ => counts
      }
    )
  let keys = ["Nominal", "Ordinal", "Interval", "Ratio"]
  let counts = keys->Array.keepMap(k => counts->String.Map.get(k)->Option.map(v => (k, v)))
  Array.concat(
    if counts->Array.length === 0 {
      []
    } else {
      [("Quantity Scales", "")]
    },
    counts->Array.flatMap(((k, (v1, v2))) =>
      [
        (indent ++ k, Int.toString(v1 + v2)),
        (indent ++ indent ++ "Concept", Int.toString(v1)),
        (indent ++ indent ++ "Graphic", Int.toString(v2)),
      ]->Array.keep(((_, v)) => v !== "0")
    ),
  )
}

let countIdioms = intelligence =>
  switch intelligence {
  | None => []
  | Some(intelligence) => {
      let idiomCount = Array.length(intelligence.Intelligence_Intf.Response.insights)
      if idiomCount == 0 {
        []
      } else {
        let inc = o => o->Option.map(v => v + 1)->Option.getWithDefault(1)->Some
        let counts =
          intelligence.Intelligence_Intf.Response.insights->Array.reduce(String.Map.empty, (
            counts,
            i,
          ) => {
            let msg = ModelInsight.message(i)
            if msg->String.includes(" Pick ") {
              counts->String.Map.update("Pick", inc)
            } else if msg->String.includes(" Filter ") {
              counts->String.Map.update("Filter", inc)
            } else if msg->String.includes(" For-each ") {
              counts->String.Map.update("For-each", inc)
            } else if msg->String.includes(" Reduce ") {
              counts->String.Map.update("Reduce", inc)
            } else if msg->String.includes("Sum R-dimension ") {
              counts->String.Map.update("Sum Dims", inc)
            } else if msg->String.includes("Product R-dimension ") {
              counts->String.Map.update("Product Dims", inc)
            } else if msg->String.includes("Explicit Coordinate System ") {
              counts->String.Map.update("Expl. Co-Sys", inc)
            } else if msg->String.includes("Implicit Coordinate System ") {
              counts->String.Map.update("Impl. Co-Sys", inc)
            } else {
              counts
            }
          })
        let keys = [
          "Pick",
          "Filter",
          "For-each",
          "Reduce",
          "Sum Dims",
          "Product Dims",
          "Expl. Co-Sys",
          "Impl. Co-Sys",
        ]
        let counts = keys->Array.keepMap(k => counts->String.Map.get(k)->Option.map(v => (k, v)))
        Array.concat(
          [("Idioms", Int.toString(idiomCount))],
          counts->Array.map(((k, v)) => (indent ++ k, Int.toString(v))),
        )
      }
    }
  }

let completenessRatio = (slots, links) => {
  let n = slots->Gid.Map.size
  let mst_count = Float.fromInt(n - 1)
  let complete_count = Float.fromInt(n * (n - 1) / 2)
  if complete_count == mst_count {
    // Either 1 or 2 schemas
    let l = Array.length(links)
    if (n == 1 && l == 0) || (n == 2 && l == 1) {
      Js.Float.toFixedWithPrecision(1.0, ~digits=4)
    } else {
      Js.Float.toFixedWithPrecision(-1.0, ~digits=4)
    }
  } else {
    Js.Float.toFixedWithPrecision(
      (Array.length(links)->Float.fromInt -. mst_count) /. (complete_count -. mst_count),
      ~digits=4,
    )
  }
}

let statistics = data => {
  let length = data->Array.length->Int.toFloat
  let mean = data->Array.reduce(0, (a, b) => a + b)->Int.toFloat /. length
  let variance =
    data
    ->Array.map(x => Js.Math.pow_float(~base=Int.toFloat(x) -. mean, ~exp=2.0))
    ->Array.reduce(0., (a, b) => a +. b) /. length
  let stddev = Js.Math.sqrt(variance)
  let min = data->Array.reduce(data[0]->Option.getWithDefault(0), Int.min)
  let max = data->Array.reduce(0, Int.max)
  {
    "mean": mean,
    "stddev": stddev,
    "min": min,
    "max": max,
  }
}

let branchingFactor = model => {
  let bfs = []
  let seen = ref(Gid.Set.empty)
  let rec f = s => {
    let id = Schema.id(s)
    if !(seen.contents->Gid.Set.has(id)) {
      seen := seen.contents->Gid.Set.add(id)
      let children = Schema.children(s)
      bfs->Js.Array2.push(List.length(children))->ignore
      children->List.forEach(f)
    }
  }
  f(Model.root(model))
  let bfs_nonzero = bfs->Array.filter(bf => bf > 0)
  let stats = statistics(bfs)
  let stats_nonzero = statistics(bfs_nonzero)
  [
    ("Branching factor", ""),
    (indent ++ "Mean", stats["mean"]->Js.Float.toFixedWithPrecision(~digits=4)),
    (indent ++ "StdDev", stats["stddev"]->Js.Float.toFixedWithPrecision(~digits=4)),
    (indent ++ "Min", stats["min"]->Int.toString),
    (indent ++ "Max", stats["max"]->Int.toString),
    (
      indent ++ "Mean (ignoring leaves)",
      stats_nonzero["mean"]->Js.Float.toFixedWithPrecision(~digits=4),
    ),
    (
      indent ++ "StdDev (ignoring leaves)",
      stats_nonzero["stddev"]->Js.Float.toFixedWithPrecision(~digits=4),
    ),
    (indent ++ "Min (ignoring leaves)", stats_nonzero["min"]->Int.toString),
  ]
}

// TODO: This finds each leaf via it's shortest path *only*. Do we actually want *every* path?
let depth = model => {
  let depths = []
  let seen = ref(Gid.Set.empty)
  let queue = Belt.MutableQueue.make()
  queue->Belt.MutableQueue.add((Model.root(model), 0))
  while !(queue->Belt.MutableQueue.isEmpty) {
    let (s, d) = queue->Belt.MutableQueue.popExn
    let id = Schema.id(s)
    if !(seen.contents->Gid.Set.has(id)) {
      seen := seen.contents->Gid.Set.add(id)
      let children = Schema.children(s)
      if List.isEmpty(children) {
        depths->Js.Array2.push(d)->ignore
      } else {
        children->List.forEach(s' => queue->Belt.MutableQueue.add((s', d + 1)))
      }
    }
  }
  let stats = statistics(depths)
  [
    ("Depth", ""),
    (indent ++ "Min", stats["min"]->Int.toString),
    (indent ++ "Max", stats["max"]->Int.toString),
    (indent ++ "Mean", stats["mean"]->Js.Float.toFixedWithPrecision(~digits=4)),
    (indent ++ "StdDev", stats["stddev"]->Js.Float.toFixedWithPrecision(~digits=4)),
  ]
}

// Note this only counts the *excess* paths --
// if there are three ways to reach a schema, we count "two" excess paths.
let parallelPaths = model => {
  let seen = ref(Gid.Map.empty())
  let queue = Belt.MutableQueue.make()
  queue->Belt.MutableQueue.add(Model.root(model))
  while !(queue->Belt.MutableQueue.isEmpty) {
    let s = queue->Belt.MutableQueue.popExn
    let id = Schema.id(s)
    if !(seen.contents->Gid.Map.has(id)) {
      seen := seen.contents->Gid.Map.set(id, 1)
    } else {
      seen := seen.contents->Gid.Map.update(id, Option.map(_, v => v + 1))
    }
    s->Schema.children->List.forEach(s' => queue->Belt.MutableQueue.add(s'))
  }
  let paths =
    seen.contents
    ->Gid.Map.values
    ->Array.keepMap(v =>
      if v >= 1 {
        Some(v - 1)
      } else {
        None
      }
    )
  if Array.length(paths) === 0 {
    []
  } else {
    let stats = statistics(paths)
    [
      ("Parallel (additional) paths", ""),
      (indent ++ "Min", stats["min"]->Int.toString),
      (indent ++ "Max", stats["max"]->Int.toString),
      (indent ++ "Mean", stats["mean"]->Js.Float.toFixedWithPrecision(~digits=4)),
      (indent ++ "StdDev", stats["stddev"]->Js.Float.toFixedWithPrecision(~digits=4)),
    ]
  }
}

let compute = (slots, links, intelligence) => {
  let metrics =
    ModelMetrics.empty
    ->ModelMetrics.addMany(countNodeTypes(slots))
    ->ModelMetrics.addMany(countLinkTypes(links))
    ->ModelMetrics.addMany(countQuantityScales(slots))
    ->ModelMetrics.addMany(countIdioms(intelligence))
    ->ModelMetrics.add("Connectedness", completenessRatio(slots, links))
  switch Model.fromSlotsAndLinks(slots, links) {
  | Result.Ok(model) =>
    metrics
    ->ModelMetrics.addMany(branchingFactor(model))
    ->ModelMetrics.addMany(depth(model))
    ->ModelMetrics.addMany(parallelPaths(model))
    ->Promise.resolve
  | Result.Error([], []) => Promise.resolve(metrics)
  | Result.Error(_) => Promise.resolve(metrics)
  }
}
