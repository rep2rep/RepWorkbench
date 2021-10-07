let json = Node.Fs.readFileAsUtf8Sync("monstergraph.json")->Js.Json.parseExn

let monsterGraph = Schema.fromJson(json)

Testing.equal(
  "Monster graph is a valid representation",
  () => monsterGraph->Or_error.map(Schema.validate)->Or_error.okExn,
  true,
)

Testing.equal(
  "Generated JSON from monster graph is the same as what we wrote",
  () => monsterGraph->Or_error.map(Schema.toJson)->Or_error.toString(Js.Json.stringify),
  Or_error.create(json)->Or_error.toString(Js.Json.stringify),
)
