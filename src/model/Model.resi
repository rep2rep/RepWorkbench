type t

let root: t => Schema.t
let schemas: t => array<Schema.t>
let relations: t => array<SchemaRelation.t>
let validate: t => Or_error.t<unit>

let fromSlotsAndLinks: (
  Gid.Map.t<InspectorState.Schema.t>,
  array<(Gid.t, Gid.t, ModelLink.Kind.t)>,
) => Result.t<array<t>, (array<ModelError.t>, array<ModelWarning.t>)>
let toSlotsAndLinks: t => (
  Gid.Map.t<InspectorState.Schema.t>,
  array<(Gid.t, Gid.t, ModelLink.Kind.t)>,
)

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>
