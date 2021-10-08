{
  let ok = Or_error.create("Hello!")
  let err = Or_error.error_s("This is an error.")
  Testing.equal(
    "Or_error JSON round-tripping",
    () => ok->Or_error.toJson(String.toJson)->Or_error.fromJson(String.fromJson)->Or_error.valOf,
    Some(ok),
  )
  Testing.equal(
    "Or_error JSON round-tripping",
    () => err->Or_error.toJson(String.toJson)->Or_error.fromJson(String.fromJson)->Or_error.valOf,
    Some(err),
  )

  Testing.assertTrue("Or_error.isOk for ok", () => ok->Or_error.isOk)
  Testing.assertFalse("Or_error.isOk for err", () => err->Or_error.isOk)
  Testing.assertFalse("Or_error.isError for ok", () => ok->Or_error.isError)
  Testing.assertTrue("Or_error.isError for err", () => err->Or_error.isError)

  Testing.equal(
    "Or_error tagging oks works",
    () => ok->Or_error.tag("Tag!")->Or_error.tag("Again!")->Or_error.tags,
    [Error.Tag.fromString("Tag!"), Error.Tag.fromString("Again!")],
  )

  Testing.equal(
    "Or_error tagging errors works",
    () => err->Or_error.tag("Tag!")->Or_error.tag("Again!")->Or_error.tags,
    [Error.Tag.fromString("Tag!"), Error.Tag.fromString("Again!")],
  )

  Testing.equal("Or_error map ok", () => ok->Or_error.map(String.length), Or_error.create(6))
  Testing.equal("Or_error map err", () => err->Or_error.map(String.concat("Oops")), err)

  Testing.equal(
    "Or_error flatMap ok",
    () => ok->Or_error.tag("Tag!")->Or_error.flatMap(s => Or_error.create(String.length(s))),
    Or_error.create(6)->Or_error.tag("Tag!"),
  )

  Testing.equal(
    "Or_error flatMap err",
    () => err->Or_error.tag("Tag!")->Or_error.flatMap(_ => Or_error.error_s("Another error")),
    err->Or_error.tag("Tag!"),
  )

  Testing.equal(
    "Or_error all ok",
    () => Or_error.all(list{Or_error.create(1), Or_error.create(2)}),
    Or_error.create(list{1, 2}),
  )

  Testing.equal(
    "Or_error all not ok",
    () =>
      Or_error.all(list{
        Or_error.create(1),
        Or_error.error_s("X"),
        Or_error.error_s("Y"),
      })->Or_error.toString(l => l->List.toString(Int.toString)),
    "\nX\nY\n",
  )

  Testing.equal(
    "Or_error allUnit succeeds",
    () => Or_error.allUnit(list{Or_error.create(), Or_error.create()}),
    Or_error.create(),
  )

  Testing.equal(
    "Or_error allUnit fails",
    () =>
      Or_error.allUnit(list{
        Or_error.create(),
        Or_error.error_s("X"),
        Or_error.error_s("Y"),
      })->Or_error.toString(() => ""),
    "\nX\nY\n",
  )

  Testing.equal(
    "Or_error both ok",
    () => Or_error.both((Or_error.create(1), Or_error.create(2))),
    Or_error.create((1, 2)),
  )

  Testing.equal(
    "Or_error both err",
    () => Or_error.both((Or_error.error_s("X"), Or_error.error_s("Y")))->Or_error.toString(_ => ""),
    "\nX\nY\n",
  )

  Testing.equal(
    "Or_error both3 err",
    () =>
      Or_error.both3((
        Or_error.error_s("X"),
        Or_error.error_s("Y"),
        Or_error.error_s("Z"),
      ))->Or_error.toString(_ => ""),
    "\nX\nY\nZ\n",
  )
}
