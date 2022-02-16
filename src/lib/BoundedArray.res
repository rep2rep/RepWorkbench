type t<'a> = {
  limit: int,
  contents: array<'a>,
}

let truncate = (arr, limit) =>
  if Array.length(arr) > limit {
    arr->Array.sliceToEnd(-limit)
  } else {
    arr
  }

let create = limit => {limit: limit, contents: []}

let isEmpty = t => t.contents == []
let push = (t, a) => {
  ...t,
  contents: t.contents->Array.push(a)->truncate(t.limit),
}
let pop = t => {
  if isEmpty(t) {
    None
  } else {
    let (a, contents) = Array.pop(t.contents)
    Some((a, {...t, contents: contents}))
  }
}

let replaceTop = (t, a) => {
  if isEmpty(t) {
    {...t, contents: [a]}
  } else {
    let contents =
      t.contents->Array.slice(~offset=0, ~len=Array.length(t.contents) - 1)->Array.push(a)
    {...t, contents: contents}
  }
}
