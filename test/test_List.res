Testing.equal("mapPartial empty", () => List.mapPartial(List.empty, Option.some), List.empty)

{
  let l = list{1, 3, 6, 2, 900}
  Testing.equal("mapPartial all some", () => List.mapPartial(l, Option.some), l)
}

Testing.equal(
  "mapPartial all none",
  () => List.mapPartial(list{1, 4, 2, 7, 182, -4}, _ => None),
  List.empty,
)

Testing.equal(
  "mapPartial general",
  () =>
    List.mapPartial(list{1, 2, 3, 4, 5, 6, 7, 8}, x =>
      if mod(x, 2) == 0 {
        Some(x)
      } else {
        None
      }
    ),
  list{2, 4, 6, 8},
)

{
  let l = list{3, 2, 4, 100, 7}
  Testing.equal(
    "JSON round-tripping",
    () => {
      l
      ->List.to_JSON(x => Int.toFloat(x)->Js.Json.number)
      ->List.of_JSON(j => Js.Json.decodeNumber(j)->Option.map(Int.fromFloat))
      ->Option.getExn
    },
    l,
  )
}
