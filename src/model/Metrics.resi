type nodes = RepWeb.Gid.Map.t<InspectorState.Schema.t>
type links = array<(RepWeb.Gid.t, RepWeb.Gid.t, ModelLink.Kind.t)>

let compute: (nodes, links, option<Intelligence_Intf.Response.t>) => Promise.t<ModelMetrics.t>
