let json = Node.Fs.readFileAsUtf8Sync("monstergraph.json")->Js.Json.parseExn

let monsterGraph = Schema.fromJson(json)

Testing.equal(
  "Monster graph is a valid representation",
  () => monsterGraph->Option.map(Schema.validate),
  Some(true),
)

Testing.equal(
  "Generated JSON from monster graph is the same as what we wrote",
  () => monsterGraph->Option.map(Schema.toJson)->Option.map(Js.Json.stringify),
  Some(json)->Option.map(Js.Json.stringify),
)
