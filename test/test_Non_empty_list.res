Testing.equal(
  "Reversing a singleton",
  () => Non_empty_list.singleton("test")->Non_empty_list.reverse,
  Non_empty_list.singleton("test"),
)

Testing.equal(
  "Reversing several items",
  () => Non_empty_list.of_list(list{1, 2, 3})->Option.map(Non_empty_list.reverse),
  Non_empty_list.of_list(list{3, 2, 1}),
)

{
  let l = Non_empty_list.of_list(list{3, 2, 4, 100, 7})->Option.getExn
  Testing.equal(
    "JSON round-tripping",
    () => {
      l
      ->Non_empty_list.to_JSON(x => Int.toFloat(x)->Js.Json.number)
      ->Non_empty_list.of_JSON(j => Js.Json.decodeNumber(j)->Option.map(Int.fromFloat))
      ->Option.getExn
    },
    l,
  )
}
