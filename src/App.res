module App = {
  type state = ModelState.t
  type action = ModelAction.t

  let init = ModelState.load()->Option.getWithDefault(ModelState.init)
  let reducer = (state, action) => {
    let newState = ModelAction.dispatch(state, action)
    ModelState.save(newState)
    newState
  }

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
    let selectionChange = (~oldSelection as _, ~newSelection) =>
      dispatch(ModelAction.Selection(newSelection))
    let linkNodes = _ => {
      let ids = ModelState.selection(state).nodes
      switch ids {
      | [source] => dispatch(ModelAction.Connect(source, source))
      | [source, target] => dispatch(ModelAction.Connect(source, target))
      | _ => ()
      }
    }
    let deleteNodes = _ =>
      ModelState.selection(state).nodes->Array.forEach(id => dispatch(ModelAction.Delete(id)))
    let movedNodes = (nodeId, ~x, ~y) => dispatch(ModelAction.Move(nodeId, x, y))

    <main>
      <div className="graph-header">
        <button onClick={addRepNode}> {React.string("Add Representation Node")} </button>
        <button onClick={addSchNode}> {React.string("Add Scheme Node")} </button>
        <button onClick={addDimNode}> {React.string("Add Dimension Node")} </button>
        <button onClick={addTokNode}> {React.string("Add Token Node")} </button>
        <button onClick={linkNodes}> {React.string("Link")} </button>
        <button onClick={deleteNodes}> {React.string("Delete")} </button>
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
          onSelectionChange={selectionChange}
          onNodePositionChange={movedNodes}
        />
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
