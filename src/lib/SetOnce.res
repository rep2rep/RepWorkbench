type t<'a> = {mutable contents: option<'a>}

let create = () => {contents: None}

let set = (t, v) =>
  t.contents = switch t.contents {
  | Some(x) => Some(x)
  | None => Some(v)
  }

let get = t => t.contents
let getExn = t => get(t)->Option.getExn
