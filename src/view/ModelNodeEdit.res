// This file is a bit of a HACK!
// Trying to combine JS with React has proven a pain.
// This is a weird workaround

let __underlying_global: ref<option<(Gid.t, string, string, string) => unit>> = ref(None)
let __underlying_local: ref<option<ModelSelection.t => unit>> = ref(None)

let setGlobal: ((Gid.t, string, string, string) => unit) => unit = f =>
  __underlying_global := Some(f)
let setLocal: (ModelSelection.t => unit) => unit = f => __underlying_local := Some(f)

let callGlobal = (id, a, b, c) => Option.iter(__underlying_global.contents, f => f(id, a, b, c))
let callLocal = selection => Option.iter(__underlying_local.contents, f => f(selection))

let clearLocal = () => __underlying_local := None
