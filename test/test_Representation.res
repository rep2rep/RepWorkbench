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
  Schema.Representation.uuid: Gid.fromString("4"),
  domain: "The domain",
  display: "Graphic reference",
  tokens: list{},
  dimensions: list{},
  schemes: list{},
  subrepresentations: list{},
}

// This is false because you are meant to have at least one child.
Testing.assertFalse("Valid representation", () => Schema.Representation.validate(r)->Or_error.isOk)

Testing.equal("JSON is as expected", () => Schema.Representation.toJson(r), json)

Testing.equal(
  "JSON round-tripping",
  () => {
    Schema.Representation.toJson(r)->Schema.Representation.fromJson->Or_error.valOf
  },
  Some(r),
)
