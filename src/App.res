module App = {
  type state = ModelState.t
  type action = ModelAction.t

  let init = ModelState.init
  let reducer = (state, action) => ModelAction.dispatch(state, action)

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)

    let addRepNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Representation))
    let addSchNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Scheme))
    let addDimNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Dimension))
    let addTokNode = _ => dispatch(ModelAction.Create(0.0, 0.0, ModelNode.Kind.Token))

    <main>
      <div className="graph-header">
        <button onClick={addRepNode}> {React.string("Add Representation Node")} </button>
        <button onClick={addSchNode}> {React.string("Add Scheme Node")} </button>
        <button onClick={addDimNode}> {React.string("Add Dimension Node")} </button>
        <button onClick={addTokNode}> {React.string("Add Token Node")} </button>
      </div>
      <div
        className="container"
        style={ReactDOM.Style.make(
          ~height="calc(100%-72px)",
          ~fontSize="0.9rem",
          ~fontFamily="sans-serif",
          (),
        )}>
        <ReactD3Graph.Graph id={"modelGraph"} data={ModelState.data(state)} />
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
