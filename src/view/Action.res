type t =
  | CreateNode(ModelNode.Kind.t, Uuid.t)
  | DeleteNode(Uuid.t)

let create = (state, _, _) => state
let delete = (state, _) => state

let dispatch = (state, action) =>
  switch action {
  | CreateNode(kind, id) => create(state, kind, id)
  | DeleteNode(id) => delete(state, id)
  }
