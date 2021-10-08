type t

let root: t => Schema.t
let relations: t => list<SchemaRelation.t>
let validate: t => Or_error.t<unit>

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>
