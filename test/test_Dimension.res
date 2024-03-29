let json = %raw(`{
"start": "2",
"2": {
"concept": "Concept",
"concept_scale": "Interval",
"concept_attributes": ["attribute"],
"graphic": null,
"graphic_scale": "Ratio",
"graphic_attributes": [],
"function": "Auxiliary",
"scope": "Local",
"explicit": false,
"dimensions": [],
"tokens": [ "1" ],
"anchored_dimensions": [],
"organisation": "Organisation"
},
"1": Test_Token.json["1"]
}`)

let d = {
  Schema.Dimension.id: Gid.fromString("2"),
  concept: "Concept",
  concept_scale: Quantity_scale.Interval,
  concept_attributes: list{"attribute"},
  graphic: None,
  graphic_scale: Quantity_scale.Ratio,
  graphic_attributes: list{},
  function: Function.Auxiliary,
  scope: Scope.Local,
  explicit: false,
  dimensions: list{},
  tokens: list{Test_Token.t},
  anchored_dimensions: list{},
  organisation: "Organisation",
}

Testing.assertTrue("Valid dimension", () => () == Schema.Dimension.validate(d)->Or_error.okExn)

Testing.equal("Dimension JSON is as expected", () => Schema.Dimension.toJson(d), json)

Testing.equal(
  "Dimension JSON round-tripping",
  () => Schema.Dimension.toJson(d)->Schema.Dimension.fromJson->Or_error.valOf,
  Some(d),
)
