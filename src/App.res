module App = {
  type state = State.t
  type action =
    | ModelAction(ModelAction.t)
    | InspectorAction

  let saveKey = "RepNotation:ModelState"
  let init = {
    State.models: [
      {
        State.Model.name: "Model",
        model: ModelState.load(saveKey)->Option.getWithDefault(ModelState.empty),
        slots: Belt.Map.String.empty,
      },
    ],
    currentModel: Some(0),
  }
  let reducer = (state, action) => {
    let newState = switch action {
    | ModelAction(action) =>
      state->State.updateModel(ModelAction.dispatch(state->State.modelState, action))
    | InspectorAction => state
    }
    ModelState.save(saveKey, newState->State.modelState)
    newState
  }

  let config = ReactD3Graph.Config.create(
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)
    let dispatchM = a => dispatch(ModelAction(a))

    let addRepNode = _ => dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Representation))
    let addSchNode = _ => dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Scheme))
    let addDimNode = _ => dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Dimension))
    let addTokNode = _ => dispatchM(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Token))
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
      ->Array.forEach(id => dispatchM(ModelAction.Delete(id)))
    let movedNodes = (nodeId, ~x, ~y) => dispatchM(ModelAction.Move(nodeId, x, y))

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
