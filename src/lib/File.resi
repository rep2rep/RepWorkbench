type t

let lastModified: t => float
let name: t => string
let size: t => int
let type_: t => string

let text: t => Js.Promise.t<string>
let arrayBuffer: t => Js.Promise.t<Js.TypedArray2.ArrayBuffer.t>
