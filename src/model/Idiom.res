type t<_> = (Gid.Map.t<ModelNode.Kind.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

type sumDimension
type prodDimension
type pickCollection
type filterCollection
type forEachCollection
type reduceCollection
type explicitCoordinateSystem
type implicitCoordinateSystem

let sumDimension = nSubs => {
  let parent = Gid.create()
  let children = Array.range(1, nSubs)->Array.map(_ => Gid.create())
  let links = children->Array.map(child => (parent, child, ModelLink.Kind.Hierarchy))
  (
    Array.concatMany([
      [(parent, ModelNode.Kind.Dimension)],
      children->Array.map(id => (id, ModelNode.Kind.Dimension)),
    ])->Gid.Map.fromArray,
    links,
  )
}

let prodDimension = nProds => {
  let parents = Array.range(1, nProds)->Array.map(_ => Gid.create())
  let child = Gid.create()
  let links = parents->Array.map(p => (p, child, ModelLink.Kind.Hierarchy))
  (
    Array.concatMany([
      parents->Array.map(id => (id, ModelNode.Kind.Dimension)),
      [(child, ModelNode.Kind.Dimension)],
    ])->Gid.Map.fromArray,
    links,
  )
}

let implicitCoordinateSystem = nDims => {
  let parent = Gid.create()
  let children = Array.range(1, nDims)->Array.map(_ => Gid.create())
  let links = children->Array.map(child => (parent, child, ModelLink.Kind.Hierarchy))
  (
    Array.concatMany([
      [(parent, ModelNode.Kind.Scheme)],
      children->Array.map(id => (id, ModelNode.Kind.Dimension)),
    ])->Gid.Map.fromArray,
    links,
  )
}

let explicitCoordinateSystem = ((coords, data)) => {
  let root = Gid.create()
  let coordRoot = Gid.create()
  let coordChildren = Array.range(1, coords)->Array.map(_ => Gid.create())
  let dataChildren = Array.range(1, data)->Array.map(_ => Gid.create())
  let coordLinks = coordChildren->Array.map(child => (coordRoot, child, ModelLink.Kind.Hierarchy))
  let rootLinks = Array.concat(
    [(root, coordRoot, ModelLink.Kind.Hierarchy)],
    dataChildren->Array.map(child => (root, child, ModelLink.Kind.Hierarchy)),
  )
  (
    Array.concatMany([
      [(root, ModelNode.Kind.Scheme), (coordRoot, ModelNode.Kind.Scheme)],
      coordChildren->Array.map(id => (id, ModelNode.Kind.Dimension)),
      dataChildren->Array.map(id => (id, ModelNode.Kind.Dimension)),
    ])->Gid.Map.fromArray,
    Array.concat(coordLinks, rootLinks),
  )
}
