module App = {
  type state = ModelState.t
  type action = ModelAction.t

  let init = ModelState.init
  let reducer = (state, action) => ModelAction.dispatch(state, action)

  let config = ReactD3Graph.Config.create(
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)

    let addRepNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Representation))
    let addSchNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Scheme))
    let addDimNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Dimension))
    let addTokNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Token))
    let clickGraph = _ => dispatch(ModelAction.ClearSelection)
    let clickNode = (event, nodeId, _node) =>
      switch ReactEvent.Pointer.shiftKey(event) {
      | true =>
        if state->ModelState.nodeIsSelected(nodeId) {
          dispatch(ModelAction.Deselect(nodeId))
        } else {
          dispatch(ModelAction.Select(nodeId))
        }
      | _ => {
          dispatch(ModelAction.ClearSelection)
          dispatch(ModelAction.Select(nodeId))
        }
      }
    let linkNodes = _ => {
      let ids = ModelState.selectedNodeIds(state)
      let source = ids[0]
      let target = ids[1]
      source->Option.iter(source =>
        target->Option.iter(target => dispatch(ModelAction.Connect(source, target)))
      )
    }

    <main>
      <div className="graph-header">
        <button onClick={addRepNode}> {React.string("Add Representation Node")} </button>
        <button onClick={addSchNode}> {React.string("Add Scheme Node")} </button>
        <button onClick={addDimNode}> {React.string("Add Dimension Node")} </button>
        <button onClick={addTokNode}> {React.string("Add Token Node")} </button>
        <button onClick={linkNodes}> {React.string("Link")} </button>
      </div>
      <div
        className="container"
        style={ReactDOM.Style.make(
          ~height="calc(100%-72px)",
          ~fontSize="0.9rem",
          ~fontFamily="sans-serif",
          (),
        )}>
        <ReactD3Graph.Graph
          id={"modelGraph"}
          data={ModelState.data(state)}
          config
          onClickGraph={clickGraph}
          onClickNode={clickNode}
        />
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
