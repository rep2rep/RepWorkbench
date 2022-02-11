type t

@get external lastModified: t => float = "lastModified"
@get external name: t => string = "name"
@get external size: t => int = "size"
@get external type_: t => string = "type"

@send external text: t => Js.Promise.t<string> = "text"
@send external arrayBuffer: t => Js.Promise.t<Js.TypedArray2.ArrayBuffer.t> = "arrayBuffer"
