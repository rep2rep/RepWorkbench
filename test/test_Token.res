let json = %raw(`{
"concept": "Some concept",
"concept_type": "Concept type",
"graphic": "Graphic reference",
"graphic_type": "Graphic type",
"level": "Atomic",
"function": "Semantic",
"explicit": true,
"sub_tokens": [],
"anchored_tokens": [],
"anchored_dimensions": [],
"anchored_schemes": []
}`)


let t = {
  Schema.Token.concept: "Some concept",
  concept_type: "Concept type",
  graphic: Some("Graphic reference"),
  graphic_type: "Graphic type",
  level: Schema.Token.Level.Atomic,
  function: Function.Semantic,
  explicit: true,
  sub_tokens: List.empty,
  anchored_tokens: List.empty,
  anchored_dimensions: List.empty,
  anchored_schemes: List.empty,
}

Testing.assertTrue("Valid token", () => Schema.Token.validate(t))

Testing.equal("JSON is as expected", () => Schema.Token.toJson(t), json)

Testing.equal(
  "JSON round-tripping",
  () => Schema.Token.toJson(t)->Schema.Token.fromJson->Option.getExn,
  t,
)
