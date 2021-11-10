type t = Create(float, float)

let createNewNode = (state, x, y) => {
  let payload = ModelState.NodePayload.create("Testing!")
  let config = ReactD3Graph.Node.Config.create()
  let node = ReactD3Graph.Node.create(
    ~id=Uuid.create()->Uuid.toString->ReactD3Graph.Node.Id.ofString,
    ~payload,
    ~config,
    ~x,
    ~y,
    (),
  )
  ModelState.addNode(state, node)
}

let dispatch = (state, action) =>
  switch action {
  | Create(x, y) => createNewNode(state, x, y)
  }
