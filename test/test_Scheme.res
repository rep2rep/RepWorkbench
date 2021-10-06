let json = %raw(`{
"start": "3",
"3": {
"concept_structure": "Concept structure",
"concept_type": "Concept type",
"graphic_structure": "Graphic",
"graphic_type": "Graphic type",
"function": "Arbitrary",
"explicit": true,
"scope": "Local",
"tokens": ["1"],
"dimensions": ["2"],
"schemes": [],
"organisation": "Organisation"
},
"2": Test_Dimension.json["2"],
"1": Test_Token.json["1"]
}`)

let s = {
  Schema.Scheme.uuid: Uuid.fromString("3"),
  concept_structure: "Concept structure",
  concept_type: "Concept type",
  graphic_structure: Some("Graphic"),
  graphic_type: "Graphic type",
  function: Function.Arbitrary,
  explicit: true,
  scope: Scope.Local,
  tokens: List.singleton(Test_Token.t),
  dimensions: Non_empty_list.singleton(Test_Dimension.d),
  schemes: List.empty,
  organisation: "Organisation",
}

Testing.assertTrue("Valid scheme", () => Schema.Scheme.validate(s))

Testing.equal(
  "Scheme JSON is as expected",
  () => Schema.Scheme.toJson(s)->Js.Json.stringify,
  json->Js.Json.stringify,
)

Testing.equal("Can read Scheme JSON", () => Schema.Scheme.fromJson(json)->Or_error.valOf, Some(s))

Testing.equal(
  "Scheme JSON round-tripping",
  () => Schema.Scheme.toJson(s)->Schema.Scheme.fromJson->Or_error.valOf,
  Some(s),
)
