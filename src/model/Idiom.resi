module Node: {
  type t =
    | Any
    | Representation
    | Scheme
    | Dimension
    | Token({is_class: bool})
    | Placeholder
}

module Link: {
  type t =
    | Any
    | Hierarchy
    | Anchor
    | Relation
    | Overlap
    | Disjoint
    | Generic
}

type t<'kind>
type graph = (Gid.Map.t<InspectorState.Schema.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

type sumDimension
type prodDimension
type pickCollection
type filterCollection
type forEachCollection
type reduceCollection
type explicitCoordinateSystem
type implicitCoordinateSystem

let nodes: t<'a> => Gid.Map.t<Node.t>
let links: t<'a> => array<(Gid.t, Gid.t, Link.t)>

// Given an isomorphism between the idiom and graph,
// extract the largest idiom-matching subgraph of the graph
let matchMaximal: (t<'a>, graph, Gid.Map.t<Gid.t>) => graph

let sumDimension: t<sumDimension>
let prodDimension: t<prodDimension>

let pickCollection: t<pickCollection>
let filterCollection: t<filterCollection>
let forEachCollection: t<forEachCollection>
let reduceCollection: t<reduceCollection>

let implicitCoordinateSystem: t<implicitCoordinateSystem>
let explicitCoordinateSystem: t<explicitCoordinateSystem>
