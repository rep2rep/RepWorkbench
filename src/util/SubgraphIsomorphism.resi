type graph<'schema, 'link> = (Gid.Map.t<'schema>, array<(Gid.t, Gid.t, 'link)>)

// Given a whole graph to search, and the graph you are trying to find,
// plus ways to determine the equivalence of schemas and links,
// Return the all possible schemas and links from the whole graph that match the subgraph.
let findIsomorphism: (
  ~whole: graph<'s, 'l>,
  ~find: graph<'t, 'm>,
  ~equiv_schemas: ('s, 't) => bool,
  ~equiv_links: ('l, 'm) => bool,
  ~onFind: graph<'s, 'l> => unit,
) => unit
