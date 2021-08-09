module type S = {
  type t
  let validate : t => bool
}
