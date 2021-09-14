let json = %raw(`{
"concept_structure": "Concept structure",
"concept_type": "Concept type",
"graphic_structure": "Graphic",
"graphic_type": "Graphic type",
"function": "Arbitrary",
"explicit": true,
"scope": "Local",
"tokens": [],
"dimensions": [Test_Dimension.json],
"schemes": [],
"organisation": "Organisation"
}`)

let s = {
  Schema.Scheme.concept_structure: "Concept structure",
  concept_type: "Concept type",
  graphic_structure: Some("Graphic"),
  graphic_type: "Graphic type",
  function: Function.Arbitrary,
  explicit: true,
  scope: Scope.Local,
  tokens: List.empty,
  dimensions: Non_empty_list.singleton(Test_Dimension.d),
  schemes: List.empty,
  organisation: "Organisation",
}

Testing.assertTrue("Valid scheme", () => Schema.Scheme.validate(s))

Testing.equal("JSON is as expected", () => Schema.Scheme.toJson(s), json)

Testing.equal("Can read JSON", () => Schema.Scheme.fromJson(json), Some(s))

Testing.equal("JSON round-tripping", () => Schema.Scheme.toJson(s)->Schema.Scheme.fromJson, Some(s))
