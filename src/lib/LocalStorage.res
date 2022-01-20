type key = string
type data = string

type t = {length: int}
@val external t: t = "localStorage"

@send external _setItem: (t, key, data) => unit = "setItem"
@send external _getItem: (t, key) => option<data> = "getItem"
@send external _removeItem: (t, key) => unit = "removeItem"
@send external _clear: (t, unit) => unit = "clear"

let setItem = _setItem(t)
let getItem = _getItem(t)
let removeItem = _removeItem(t)
let clear = _clear(t)
let length = () => t.length
