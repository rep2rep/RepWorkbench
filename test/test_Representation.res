let json = %raw(`{
"start": "4",
"4": {
"domain": "The domain",
"display": "Graphic reference",
"tokens": [],
"dimensions": [],
"schemes": [],
"subrepresentations": []
}} `)

let r = {
  Schema.Representation.uuid: Uuid.fromString("4"),
  domain: "The domain",
  display: "Graphic reference",
  tokens: list{},
  dimensions: list{},
  schemes: list{},
  subrepresentations: list{},
}

// This is false because you are meant to have at least one child.
Testing.assertFalse("Valid representation", () => Schema.Representation.validate(r))

Testing.equal("JSON is as expected", () => Schema.Representation.toJson(r), json)

Testing.equal(
  "JSON round-tripping",
  () => {
    Schema.Representation.toJson(r)->Schema.Representation.fromJson
  },
  Some(r),
)
