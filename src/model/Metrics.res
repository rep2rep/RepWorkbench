type nodes = RepWeb.Gid.Map.t<InspectorState.Schema.t>
type links = array<(RepWeb.Gid.t, RepWeb.Gid.t, ModelLink.Kind.t)>

let compute = (slots, links) =>
  switch Model.fromSlotsAndLinks(slots, links) {
  | Result.Ok(model) => Promise.resolve(Some(ModelMetrics.empty))
  | Result.Error(_) => Promise.resolve(None)
  }
