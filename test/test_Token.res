let json = %raw(`{
"start": "1",
"1": {
"concept": "Some concept",
"graphic": "Graphic reference",
"is_class": false,
"function": "Semantic",
"explicit": true,
"sub_tokens": [],
"anchored_tokens": [],
"anchored_dimensions": [],
"anchored_schemes": [],
"anchored_representations": [],
}}`)

let t = {
  Schema.Token.id: Gid.fromString("1"),
  concept: "Some concept",
  graphic: Some("Graphic reference"),
  is_class: false,
  function: Function.Semantic,
  explicit: true,
  sub_tokens: List.empty,
  anchored_tokens: List.empty,
  anchored_dimensions: List.empty,
  anchored_schemes: List.empty,
  anchored_representations: List.empty,
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
