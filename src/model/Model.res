module Conv = {
  let schema_to_links = (schema: Schema.t) =>
    switch schema {
    | Schema.Representation(r) =>
      [
        r.tokens->List.map(t => (r.id, t.id, ModelLink.Kind.Hierarchy)),
        r.dimensions->List.map(d => (r.id, d.id, ModelLink.Kind.Hierarchy)),
        r.schemes->List.map(s => (r.id, s.id, ModelLink.Kind.Hierarchy)),
        r.subrepresentations->List.map(r' => (r.id, r'.id, ModelLink.Kind.Hierarchy)),
      ]->Array.flatMap(List.toArray)
    | Schema.Scheme(s) =>
      [
        s.tokens->List.map(t => (s.id, t.id, ModelLink.Kind.Hierarchy)),
        s.dimensions
        ->Non_empty_list.map(d => (s.id, d.id, ModelLink.Kind.Hierarchy))
        ->Non_empty_list.toList,
        s.schemes->List.map(s' => (s.id, s'.id, ModelLink.Kind.Hierarchy)),
      ]->Array.flatMap(List.toArray)
    | Schema.Dimension(d) =>
      [
        d.dimensions->List.map(d' => (d.id, d'.id, ModelLink.Kind.Hierarchy)),
        d.tokens
        ->Non_empty_list.map(t => (d.id, t.id, ModelLink.Kind.Hierarchy))
        ->Non_empty_list.toList,
      ]->Array.flatMap(List.toArray)
    | Schema.Token(t) =>
      [
        t.sub_tokens->List.map(t' => (t.id, t'.id, ModelLink.Kind.Hierarchy)),
        t.anchored_tokens->List.map(ta => (t.id, ta.id, ModelLink.Kind.Anchor)),
        t.anchored_dimensions->List.map(da => (t.id, da.id, ModelLink.Kind.Anchor)),
        t.anchored_schemes->List.map(sa => (t.id, sa.id, ModelLink.Kind.Anchor)),
      ]->Array.flatMap(List.toArray)
    }

  let representation_to_slots = (
    rep: Schema.Representation.t,
  ) => InspectorState.Schema.Representation({
    InspectorState.Representation.domain: rep.domain,
    display: rep.display,
    notes: "Automatically converted from verified model.",
  })

  let scheme_to_slots = (sch: Schema.Scheme.t) => InspectorState.Schema.Scheme({
    InspectorState.Scheme.concept_structure: sch.concept_structure,
    graphic_structure: sch.graphic_structure->Option.getWithDefault("#"),
    function: Some(sch.function),
    explicit: Some(sch.explicit),
    scope: Some(sch.scope),
    organisation: sch.organisation,
    notes: "",
  })

  let dimension_to_slots = (dim: Schema.Dimension.t) => InspectorState.Schema.Dimension({
    InspectorState.Dimension.concept: dim.concept,
    concept_scale: Some(dim.concept_scale),
    concept_attributes: dim.concept_attributes,
    graphic: dim.graphic->Option.getWithDefault("#"),
    graphic_scale: Some(dim.graphic_scale),
    graphic_attributes: dim.graphic_attributes,
    function: Some(dim.function),
    scope: Some(dim.scope),
    explicit: Some(dim.explicit),
    organisation: dim.organisation,
    notes: "",
  })

  let token_to_slots = (tok: Schema.Token.t) => InspectorState.Schema.Token({
    InspectorState.Token.concept: tok.concept,
    graphic: tok.graphic->Option.getWithDefault("#"),
    is_class: Some(tok.is_class),
    function: Some(tok.function),
    explicit: Some(tok.explicit),
    notes: "",
  })
}

type t = {
  root: Schema.t,
  relations: array<SchemaRelation.t>,
}

let root = t => t.root
let schemas = t => {
  let schemas = []
  let rec f = schema => {
    schemas->Js.Array2.push(schema)->ignore
    Schema.children(schema)->List.forEach(f)
  }
  f(t.root)
  Array.dedup(schemas)
}
let relations = t => t.relations
let validate = t =>
  switch t.root {
  | Schema.Representation(_) => Schema.validate(t.root)
  | Schema.Scheme(_) | Schema.Dimension(_) | Schema.Token(_) =>
    Or_error.both((
      Schema.validate(t.root),
      Or_error.error_s("Model root must be a Representation schema."),
    ))->Or_error.map(((s, _)) => s)
  }

let toSlotsAndLinks = t => {
  let schemas = schemas(t)
  let slots =
    schemas
    ->Array.map(schema =>
      switch schema {
      | Schema.Representation(r) => (r.id, Conv.representation_to_slots(r))
      | Schema.Scheme(s) => (s.id, Conv.scheme_to_slots(s))
      | Schema.Dimension(d) => (d.id, Conv.dimension_to_slots(d))
      | Schema.Token(t) => (t.id, Conv.token_to_slots(t))
      }
    )
    ->Gid.Map.fromArray
  let relations = t.relations->Array.map(r => {
    let (a, b) = SchemaRelation.schema(r)
    (Schema.id(a), Schema.id(b), ModelLink.Kind.Relation)
  })
  let links = schemas->Array.flatMap(Conv.schema_to_links)
  (slots, Array.concat(links, relations))
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("root", t.root->Schema.toJson),
    ("relations", t.relations->Array.toJson(SchemaRelation.toJson)),
  })->Js.Json.object_

let fromJson = json =>
  Js.Json.decodeObject(json)
  ->Or_error.fromOption_s("JSON is not a valid object (reading Model.t)")
  ->Or_error.flatMap(dict => {
    let root =
      dict
      ->Js.Dict.get("root")
      ->Or_error.fromOption_s("Unable to find model root (reading Model.t)")
      ->Or_error.flatMap(Schema.fromJson)
    let relations =
      dict
      ->Js.Dict.get("relations")
      ->Or_error.fromOption_s("Unable to find model relations (reading Model.t)")
      ->Or_error.flatMap(l =>
        l->Array.fromJson(j => root->Or_error.flatMap(root => j->SchemaRelation.fromJson(root)))
      )

    Or_error.both((root, relations))->Or_error.map(((root, relations)) => {
      root: root,
      relations: relations,
    })
  })
