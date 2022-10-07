include ModelError

let create = (~nodes, ~message, ~details, ()) => create(~nodes, ~message, ~details, ())
let suggestion = _ => None

let subsumes = (t, t') => {
  let s = t->nodes->Gid.Set.fromArray
  let s' = t'->nodes->Gid.Set.fromArray
  // s' is a subset of s
  s'->Gid.Set.subset(s) && t'->ModelError.message == t->ModelError.message
}
