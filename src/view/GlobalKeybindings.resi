module KeyBinding: {
  type t
  let create: (string, unit => unit) => t
}

let set: array<KeyBinding.t> => unit
