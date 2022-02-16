module KeyBinding = {
  type t = {
    shift: bool,
    control: bool,
    meta: bool, //Command
    alt: bool,
    key: string,
    callback: unit => unit,
  }

  let create = (keys, callback) => {
    let parts = keys->String.toUpperCase->String.split("+")
    let key = parts->Array.getExn(Array.length(parts) - 1)
    let parts = parts->Array.pop->(((_, x)) => x)->Belt.Set.String.fromArray
    {
      shift: parts->Belt.Set.String.has("SHIFT"),
      control: parts->Belt.Set.String.has("CTRL") || parts->Belt.Set.String.has("CONTROL"),
      meta: parts->Belt.Set.String.has("CMD") || parts->Belt.Set.String.has("COMMAND"),
      alt: parts->Belt.Set.String.has("ALT") ||
      parts->Belt.Set.String.has("OPT") ||
      parts->Belt.Set.String.has("OPTION"),
      key: key,
      callback: callback,
    }
  }

  let match = (t, e) =>
    t.shift == e["shiftKey"] &&
    t.control == e["ctrlKey"] &&
    t.meta == e["metaKey"] &&
    t.alt == e["altKey"] &&
    t.key == e["key"]->String.toUpperCase
}

type t<'a> = {mutable f: 'a => unit}

let t = {
  f: _ => (),
}

type window
@val external window: window = "window"
@send external addEventListener: (window, string, {..} => unit) => unit = "addEventListener"
@send external removeEventListener: (window, string, {..} => unit) => unit = "removeEventListener"

@send external preventDefault: {..} => unit = "preventDefault"
@send external stopPropagation: {..} => unit = "stopPropagation"

let set = keybindings => {
  let f = e => {
    if !e["repeat"] {
      keybindings->Array.forEach(k => {
        if k->KeyBinding.match(e) {
          e->preventDefault
          e->stopPropagation
          k.callback()
        }
      })
    }
  }
  window->removeEventListener("keydown", t.f)
  window->addEventListener("keydown", f)
  t.f = f
}
