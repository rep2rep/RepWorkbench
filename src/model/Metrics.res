type nodes = Gid.Map.t<InspectorState.Schema.t>
type links = array<(Gid.t, Gid.t, ModelLink.Kind.t)>

let countOfNodeTypes = slots => {
  let inc = v => v->Option.map(x => x + 1)->Option.getWithDefault(1)->Some
  let counts =
    slots
    ->Gid.Map.values
    ->Array.reduce(String.Map.empty, (counts, s) =>
      switch s {
      | InspectorState.Schema.Representation(_) => counts->String.Map.update("Representation", inc)
      | InspectorState.Schema.Scheme(_) => counts->String.Map.update("R-Scheme", inc)
      | InspectorState.Schema.Dimension(_) => counts->String.Map.update("R-Dimension", inc)
      | InspectorState.Schema.Token(_) => counts->String.Map.update("R-Symbol", inc)
      | InspectorState.Schema.Placeholder(_) => counts->String.Map.update("Placeholder", inc)
      }
    )
  let keys = ["Representation", "R-Scheme", "R-Dimension", "R-Symbol", "Placeholder"]
  let counts = keys->Array.keepMap(k => counts->String.Map.get(k)->Option.map(v => (k, v)))
  Array.concat(
    [("Schema counts", "")],
    counts->Array.map(((k, v)) => ("    " ++ k, Int.toString(v))),
  )
}

let compute = (slots, links) =>
  switch Model.fromSlotsAndLinks(slots, links) {
  | Result.Ok(model) => {
      let nodeCounts = countOfNodeTypes(slots)
      ModelMetrics.empty->ModelMetrics.addMany(nodeCounts)->Some->Promise.resolve
    }
  | Result.Error([], []) => Promise.resolve(Some(ModelMetrics.empty))
  | Result.Error(_) => Promise.resolve(None)
  }
