open Belt

@val external print: 'a => unit = "process.stdout.write"

module Test = {
  type t = {
    mutable comment: string,
    exec: unit => (bool, string),
  }

  let run = t => t.exec()

  let comment = t => t.comment

  let make = (comment, exec) => {comment: comment, exec: exec}
}

type t = {
  mutable tests: Js.Array.t<Test.t>,
  mutable count: int,
}

let _global = {
  tests: [],
  count: 0,
}

let addTest = t => {
  _global.tests = Js.Array.concat(_global.tests, [t])
  _global.count = _global.count + 1
}

let runAll = () => {
  let failed_count =
    _global.tests
    |> Js.Array.filter(t => {
      let (passed, output) = Test.run(t)
      t.comment = t.comment ++ "\n" ++ output
      if passed {
        print(".")
        false
      } else {
        print("!")
        true
      }
    })
    |> (x => {
      print("\n")
      x
    })
    |> Js.Array.map(t => {
      Js.Console.log("Failed test: " ++ Test.comment(t))
    })
    |> Js.Array.length

  Js.Console.log(
    Int.toString(_global.count - failed_count) ++ "/" ++ Int.toString(_global.count) ++ " passed.",
  )
}

let expect = (comment, run, string_repr) => {
  let exec = () => {
    let s = Js.String.make(run())
    if s == string_repr {
      (true, "")
    } else {
      (false, "EXPECTED: " ++ string_repr ++ "\nGOT: " ++ s)
    }
  }
  Test.make(comment, exec)->addTest
}

let assertTrue = (comment, run) => {
  let exec = () => (run(), "")
  Test.make(comment, exec)->addTest
}

let assertFalse = (comment, run) => {
  let exec = () => (!run(), "")
  Test.make(comment, exec)->addTest
}

let equal = (comment, run, expected) => {
  let exec = () => {
    let o = run()
    if expected == o {
      (true, "")
    } else {
      (false, "EXPECTED: " ++ Js.String.make(expected) ++ "\nGOT: " ++ Js.String.make(o))
    }
  }
  Test.make(comment, exec)->addTest
}
