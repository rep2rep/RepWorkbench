type t<_> = (Gid.Map.t<ModelNode.Kind.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

type sumDimension
type prodDimension
type pickCollection
type filterCollection
type forEachCollection
type reduceCollection
type explicitCoorindateSystem
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
