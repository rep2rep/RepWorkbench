let json = %raw(`{
"start": "1",
"1": {
"concept": "Some concept",
"concept_type": "Concept type",
"graphic": "Graphic reference",
"graphic_type": "Graphic type",
"is_class": false,
"level": "Atomic",
"function": "Semantic",
"explicit": true,
"sub_tokens": [],
"anchored_tokens": [],
"anchored_dimensions": [],
"anchored_schemes": []
}}`)

let t = {
  Schema.Token.uuid: Uuid.fromString("1"),
  concept: "Some concept",
  concept_type: "Concept type",
  graphic: Some("Graphic reference"),
  graphic_type: "Graphic type",
  is_class: false,
  level: Schema.Token.Level.Atomic,
  function: Function.Semantic,
  explicit: true,
  sub_tokens: List.empty,
  anchored_tokens: List.empty,
  anchored_dimensions: List.empty,
  anchored_schemes: List.empty,
}

Testing.assertTrue("Valid token", () => () == Schema.Token.validate(t)->Or_error.okExn)

Testing.equal(
  "Token JSON is as expected",
  () => Schema.Token.toJson(t)->Js.Json.stringify,
  json->Js.Json.stringify,
)

Testing.equal(
  "Token JSON round-tripping",
  () => Schema.Token.toJson(t)->Schema.Token.fromJson->Or_error.valOf,
  Some(t),
)
