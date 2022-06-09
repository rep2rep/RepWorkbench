type graph = (Gid.Map.t<InspectorState.Schema.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

// Given a whole graph to search, and the graph you are trying to find,
// plus ways to determine the equivalence of schemas and links,
// Return the all possible schemas and links from the whole graph that match the subgraph.
let findIsomorphism: (
  ~whole: graph,
  ~find: Idiom.t<_>,
  ~equiv_schemas: (InspectorState.Schema.t, Idiom.Node.t) => bool,
  ~equiv_links: (ModelLink.Kind.t, Idiom.Link.t) => bool,
  ~onFind: graph => unit,
) => unit
