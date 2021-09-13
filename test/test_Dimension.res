let json = %raw(`{
"concept": "Concept",
"concept_scale": "Interval",
"concept_type": "Concept type",
"concept_attributes": ["attribute"],
"graphic": null,
"graphic_scale": "Ratio",
"graphic_type": "Graphic type",
"graphic_attributes": [],
"function": "Auxiliary",
"scope": "Local",
"explicit": false,
"dimensions": [],
"tokens": [ Test_Token.json ]
}`)

let d = {
  Schema.Dimension.concept: "Concept",
  concept_scale: Quantity_scale.Interval,
  concept_type: "Concept type",
  concept_attributes: list{"attribute"},
  graphic: None,
  graphic_scale: Quantity_scale.Ratio,
  graphic_type: "Graphic type",
  graphic_attributes: list{},
  function: Function.Auxiliary,
  scope: Scope.Local,
  explicit: false,
  dimensions: list{},
  tokens: Non_empty_list.singleton(Test_Token.t),
}

Testing.assertTrue("Valid dimension", () => Schema.Dimension.validate(d))

Testing.equal("JSON is as expected", () => Schema.Dimension.toJson(d), json)

Testing.equal(
  "JSON round-tripping",
  () => Schema.Dimension.toJson(d)->Schema.Dimension.fromJson->Option.getExn,
  d,
)
