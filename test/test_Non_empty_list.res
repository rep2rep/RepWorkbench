Testing.equal(
  "Reversing a singleton",
  () => Non_empty_list.singleton("test")->Non_empty_list.reverse,
  Non_empty_list.singleton("test"),
)

Testing.equal(
  "Reversing several items",
  () => Non_empty_list.fromList(list{1, 2, 3})->Option.map(Non_empty_list.reverse),
  Non_empty_list.fromList(list{3, 2, 1}),
)

Testing.equal(
  "AllSome some NEL",
  () =>
    Non_empty_list.fromList(list{Some(1), Some(2), Some(3)})->Option.map(Non_empty_list.allSome),
  Some(Non_empty_list.fromList(list{1, 2, 3})),
)

Testing.equal(
  "AllSome none NEL",
  () =>
    Non_empty_list.fromList(list{Some(1), Some(2), Some(3), None})->Option.map(
      Non_empty_list.allSome,
    ),
  Some(None),
)

{
  let l = Non_empty_list.fromList(list{3, 2, 4, 100, 7})->Option.getExn
  Testing.equal(
    "JSON round-tripping",
    () => {
      l->Non_empty_list.toJson(Int.toJson)->Non_empty_list.fromJson(Int.fromJson)->Or_error.valOf
    },
    Some(l),
  )
}
