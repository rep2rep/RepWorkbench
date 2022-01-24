module App = {
  type state = State.t
  type action =
    | GlobalAction(Action.t)
    | ModelAction(ModelAction.t)

  let init = State.load()->Option.getWithDefault(State.empty)
  let reducer = (state, action) => {
    let newState = switch action {
    | GlobalAction(action) => Action.dispatch(state, action)
    | ModelAction(action) =>
      state->State.updateModel(ModelAction.dispatch(state->State.modelState, action))
    }
    State.store(newState)
    newState
  }

  let config = ReactD3Graph.Config.create(
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)
    let dispatchG = a => dispatch(GlobalAction(a))
    let dispatchM = a => dispatch(ModelAction(a))

    let addRepNode = _ => {
      let id = Uuid.create()
      dispatchG(Action.CreateNode(ModelNode.Kind.Representation, id))
      dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Representation, id))
    }
    let addSchNode = _ => {
      let id = Uuid.create()
      dispatchG(Action.CreateNode(ModelNode.Kind.Scheme, id))
      dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Scheme, id))
    }
    let addDimNode = _ => {
      let id = Uuid.create()
      dispatchG(Action.CreateNode(ModelNode.Kind.Dimension, id))
      dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Dimension, id))
    }
    let addTokNode = _ => {
      let id = Uuid.create()
      dispatchG(Action.CreateNode(ModelNode.Kind.Token, id))
      dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Token, id))
    }
    let selectionChange = (~oldSelection as _, ~newSelection) =>
      dispatchM(ModelAction.Selection(newSelection))
    let linkNodes = _ => {
      let ids = state->State.modelState->ModelState.selection->ModelSelection.nodes
      switch ids {
      | [source] => dispatchM(ModelAction.Connect(source, source))
      | [source, target] => dispatchM(ModelAction.Connect(source, target))
      | _ => ()
      }
    }
    let deleteNodes = _ =>
      state
      ->State.modelState
      ->ModelState.selection
      ->ModelSelection.nodes
      ->Array.forEach(id => {
        dispatchG(Action.DeleteNode(id))
        dispatchM(ModelAction.Delete(id))
      })
    let movedNodes = (nodeId, ~x, ~y) =>
      dispatchM(ModelAction.Move(nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString, x, y))

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
          data={state->State.modelState->ModelState.data}
          config
          onSelectionChange={selectionChange}
          onNodePositionChange={movedNodes}
        />
        <InspectorPanel id={"nodeInspector"} data={state->State.inspectorState} />
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
