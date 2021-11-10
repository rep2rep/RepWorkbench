module App = {
  type state = ModelState.t
  type action = ModelAction.t

  let init = ModelState.init
  let reducer = (state, action) => ModelAction.dispatch(state, action)

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)

    let addStartNode = _ => dispatch(ModelAction.Create(0.0, 0.0))

    <main>
      <div className="graph-header">
        <button onClick={addStartNode}> {React.string("Add Node")} </button>
      </div>
      <div className="container" style={ReactDOM.Style.make(~height="calc(100%-72px)", ())}>
        <ReactD3Graph.Graph id={"modelGraph"} data={ModelState.data(state)} />
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
