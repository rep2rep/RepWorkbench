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

let completenessRatio = (slots, links) => {
  let n = slots->Gid.Map.size
  let mst_count = Float.fromInt(n - 1)
  let complete_count = Float.fromInt(n * (n - 1) / 2)
  Js.Float.toFixedWithPrecision(
    (Array.length(links)->Float.fromInt -. mst_count) /. (complete_count -. mst_count),
    ~digits=4,
  )
}

let compute = (slots, links) => {
  let metrics =
    ModelMetrics.empty
    ->ModelMetrics.addMany(countNodeTypes(slots))
    ->ModelMetrics.addMany(countLinkTypes(links))
    ->ModelMetrics.addMany(countQuantityScales(slots))
    ->ModelMetrics.add("Connectedness", completenessRatio(slots, links))
  switch Model.fromSlotsAndLinks(slots, links) {
  | Result.Ok(model) => Promise.resolve(metrics)
  | Result.Error([], []) => Promise.resolve(metrics)
  | Result.Error(_) => Promise.resolve(metrics)
  }
}
