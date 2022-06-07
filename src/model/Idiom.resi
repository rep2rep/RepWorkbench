type t<'kind> = (Gid.Map.t<ModelNode.Kind.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

type sumDimension
type prodDimension
type pickCollection
type filterCollection
type forEachCollection
type reduceCollection
type explicitCoorindateSystem
type implicitCoordinateSystem

let sumDimension: int => t<sumDimension>
let prodDimension: int => t<prodDimension>

// let pickCollection: int => t<pickCollection>
// let filterCollection: int => t<filterCollection>
// let forEachCollection: t<forEachCollection>
// let reduceCollection: t<reduceCollection>

// let explicitCoordinateSystem: int => t<explicitCoordinateSystem>
// let implicitCoordinateSystem: (int, int) => t<implicitCoordinateSystem>
