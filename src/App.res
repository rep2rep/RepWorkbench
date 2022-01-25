module App = {
  type state = State.t
  type action =
    | GlobalAction(Action.t)
    | ModelAction(ModelAction.t)
    | InspectorAction(InspectorAction.t)

  let init = State.load()->Option.getWithDefault(State.empty)
  let reducer = (state, action) => {
    let newState = switch action {
    | GlobalAction(action) => Action.dispatch(state, action)
    | ModelAction(action) =>
      state->State.updateModel(ModelAction.dispatch(state->State.modelState, action))
    | InspectorAction(action) => {
        let (key, newSlots) = InspectorAction.dispatch(state->State.inspectorState, action)
        state->State.updateSlots(key, newSlots)
      }
    }
    State.store(newState)
    newState
  }

  let config = ReactD3Graph.Config.create(
    ~global=ReactD3Graph.Config.Global.create(~width="100%", ~height="calc(100vh - 40px)", ()),
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)
    let dispatchG = a => dispatch(GlobalAction(a))
    let dispatchM = a => dispatch(ModelAction(a))
    let dispatchI = a => dispatch(InspectorAction(a))

    let newModel = id => dispatchG(Action.NewModel(id))
    let deleteModel = id => dispatchG(Action.DeleteModel(id))
    let focusModel = id => dispatchG(Action.FocusModel(id))
    let renameModel = (id, name) => dispatchG(Action.RenameModel(id, name))
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
      | [source, target] => dispatchM(ModelAction.Connect(source, target))
      | _ => ()
      }
    }
    let unlinkNodes = _ => {
      let nodeIds = state->State.modelState->ModelState.selection->ModelSelection.nodes
      nodeIds->Array.forEach(source =>
        nodeIds->Array.forEach(target => dispatchM(ModelAction.Unlink(source, target)))
      )
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
    let slotsChange = e => {
      let selection = state->State.modelState->ModelState.selection->ModelSelection.nodes
      switch selection {
      | [nodeId] => {
          dispatchI(InspectorAction.Update(nodeId, e))
          dispatchM(ModelAction.Update(nodeId, e))
        }
      | _ => ()
      }
    }
    let save = _ =>
      ReactDOM.querySelector("[name=\"svg-container-model-graph\"]")->Option.iter(svg => {
        let serializer = XMLSerializer.create()
        let source = serializer->XMLSerializer.serializeToString(svg)
        let source = if (
          source
          ->String.match_(%re("/^<svg[^>]+xmlns=\"http\:\/\/www\.w3\.org\/2000\/svg\"/"))
          ->Option.isNone
        ) {
          source->String.replaceByRe(
            %re("/^<svg/"),
            "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\"",
          )
        } else {
          source
        }
        let source = "<?xml version=\"1.0\" standalone=\"no\"?>\r\n" ++ source
        let url = "data:image/svg+xml;charset=utf-8," ++ Js.Global.encodeURIComponent(source)
        ReactDOM.querySelector("#download-link")->Option.iter(downloadLink => {
          Js.Console.log({"url": url, "dl": downloadLink})
          %raw("downloadLink.href = url")
        })
      })

    <main
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="row",
        ~fontFamily="sans-serif",
        ~height="100%",
        (),
      )}>
      <FilePanel
        id="file-panel"
        models={State.models(state)}
        active={State.focusedId(state)}
        onCreate={newModel}
        onDelete={deleteModel}
        onSelect={focusModel}
        onChangedName={renameModel}
      />
      <div
        className="editor-panel"
        style={ReactDOM.Style.make(
          ~order="2",
          ~flexGrow="1",
          ~display="flex",
          ~flexDirection="column",
          ~height="100%",
          (),
        )}>
        <div
          className="graph-header"
          style={ReactDOM.Style.make(
            ~order="1",
            ~display="flex",
            ~alignItems="center",
            ~height="40px",
            ~borderBottom="1px solid black",
            ~padding="0 0.5rem",
            (),
          )}>
          <Button onClick={addRepNode}> {React.string("Add Representation Node")} </Button>
          <Button onClick={addSchNode}> {React.string("Add Scheme Node")} </Button>
          <Button onClick={addDimNode}> {React.string("Add Dimension Node")} </Button>
          <Button onClick={addTokNode}> {React.string("Add Token Node")} </Button>
          <Button.Separator />
          <Button onClick={linkNodes}> {React.string("Link")} </Button>
          <Button.Separator />
          <Button onClick={unlinkNodes}> {React.string("Unlink")} </Button>
          <Button.Separator />
          <Button onClick={deleteNodes}> {React.string("Delete")} </Button>
          <Button.Separator />
          <Button onClick={save}> {React.string("Prepare to download")} </Button>
          <a
            id="download-link"
            download={State.focusedName(state)->Option.getWithDefault("Untitled")}>
            {React.string("Click to download")}
          </a>
        </div>
        <div
          className="container"
          style={ReactDOM.Style.make(
            ~order="2",
            ~flexGrow="1",
            ~display="flex",
            ~flexDirection="row",
            (),
          )}>
          <ReactD3Graph.Graph
            id={"model-graph"}
            data={state->State.modelState->ModelState.data}
            config
            onSelectionChange={selectionChange}
            onNodePositionChange={movedNodes}
            style={ReactDOM.Style.make(~flexGrow="1", ())}
          />
          <InspectorPanel
            id={"node-inspector"} data={state->State.inspectorState} onChange=slotsChange
          />
        </div>
      </div>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<App />, e)
}
