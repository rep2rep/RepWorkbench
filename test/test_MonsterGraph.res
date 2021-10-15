let json = Node_fs.readFileAsUtf8Sync("monstergraph.json")->Js.Json.parseExn

let monsterGraph = Model.fromJson(json)

Testing.equal(
  "Monster graph is a valid representation",
  () => monsterGraph->Or_error.flatMap(Model.validate)->Or_error.okExn,
  (),
)

Testing.equal(
  "Generated JSON from monster graph is the same as what we wrote",
  () => monsterGraph->Or_error.map(Model.toJson)->Or_error.okExn,
  json,
)
