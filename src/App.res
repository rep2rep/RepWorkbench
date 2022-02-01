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
    | InspectorAction(action) =>
      InspectorAction.dispatch(state->State.inspectorState, action)->Array.reduce(state, (
        state,
        (key, newSlots),
      ) => state->State.updateSlots(key, newSlots))
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
    let selectionChange = (~oldSelection as _, ~newSelection) =>
      dispatchM(ModelAction.Selection(newSelection))
    let addNodeAt = (kind, ~x, ~y) => {
      let oldSelection = state->State.modelState->ModelState.selection
      let ids = oldSelection->ModelSelection.nodes
      let id = Uuid.create()
      dispatchG(Action.CreateNode(kind, id))
      dispatchM(ModelAction.Create(x, y, kind, id))
      switch ids {
      | [] => ()
      | _ => ids->Array.forEach(source => dispatchM(ModelAction.Connect(source, id)))
      }
      selectionChange(~oldSelection, ~newSelection=ModelSelection.ofNodes([id]))
    }
    let addRepNodeAt = (_, ~x, ~y) => ModelNode.Kind.Representation->addNodeAt(~x, ~y)
    let addSchNodeAt = (_, ~x, ~y) => ModelNode.Kind.Scheme->addNodeAt(~x, ~y)
    let addDimNodeAt = (_, ~x, ~y) => ModelNode.Kind.Dimension->addNodeAt(~x, ~y)
    let addTokNodeAt = (_, ~x, ~y) => ModelNode.Kind.Token->addNodeAt(~x, ~y)
    let linkNodes = _ => {
      let ids = state->State.modelState->ModelState.selection->ModelSelection.nodes
      switch ids {
      | [source, target] => dispatchM(ModelAction.Connect(source, target))
      | _ => ()
      }
    }
    let anchorNodes = _ => {
      let ids = state->State.modelState->ModelState.selection->ModelSelection.nodes
      switch ids {
      | [source, target] => dispatchM(ModelAction.Anchor(source, target))
      | _ => ()
      }
    }
    let unlinkNodes = _ => {
      let nodeIds = state->State.modelState->ModelState.selection->ModelSelection.nodes
      nodeIds->Array.forEach(source =>
        nodeIds->Array.forEach(target => dispatchM(ModelAction.Unlink(source, target)))
      )
    }
    let deleteNodes = _ => {
      let oldSelection = state->State.modelState->ModelState.selection
      oldSelection
      ->ModelSelection.nodes
      ->Array.forEach(id => {
        dispatchG(Action.DeleteNode(id))
        dispatchM(ModelAction.Delete(id))
      })
      selectionChange(~oldSelection, ~newSelection=ModelSelection.empty)
    }
    let movedNodes = (nodeId, ~x, ~y) =>
      dispatchM(ModelAction.Move(nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString, x, y))
    let duplicateNodes = _ => {
      let oldSelection = state->State.modelState->ModelState.selection
      let nodeIds = oldSelection->ModelSelection.nodes
      let nodeMap = nodeIds->Array.map(id => (id, Uuid.create()))->Uuid.Map.fromArray
      dispatchM(ModelAction.Duplicate(nodeMap))
      dispatchI(InspectorAction.Duplicate(nodeMap))
      state
      ->State.modelState
      ->ModelState.graph
      ->ModelGraph.links
      ->Array.forEach(link => {
        let source = ModelLink.source(link)
        let target = ModelLink.target(link)
        let constructor = switch ModelLink.kind(link) {
        | ModelLink.Kind.Hierarchy => (s, t) => ModelAction.Connect(s, t)
        | ModelLink.Kind.Anchor => (s, t) => ModelAction.Anchor(s, t)
        | ModelLink.Kind.Relation => (s, t) => ModelAction.Relate(s, t)
        }
        switch (nodeMap->Uuid.Map.get(source), nodeMap->Uuid.Map.get(target)) {
        | (Some(newSource), Some(newTarget)) => dispatchM(constructor(newSource, newTarget))
        | _ => ()
        }
      })
      let newSelection = Uuid.Map.values(nodeMap)->ModelSelection.ofNodes
      selectionChange(~oldSelection, ~newSelection)
    }
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
    let dump = _ => {
      let content =
        "data:text/json;charset=utf-8," ++
        state->State.dump->Js.Json.stringify->Js.Global.encodeURIComponent
      Downloader.download("RepNotationOnline.json", content)
    }

    let keybindings = Js.Dict.fromArray([
      ("r", addRepNodeAt),
      ("s", addSchNodeAt),
      ("d", addDimNodeAt),
      ("t", addTokNodeAt),
      ("c", (e, ~x as _, ~y as _) => linkNodes(e)),
      ("a", (e, ~x as _, ~y as _) => anchorNodes(e)),
      ("x", (e, ~x as _, ~y as _) => deleteNodes(e)),
      ("v", (e, ~x as _, ~y as _) => unlinkNodes(e)),
      ("Ctrl+d", (e, ~x as _, ~y as _) => duplicateNodes(e)),
    ])

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
          <Button onClick={addRepNodeAt(_, ~x=0., ~y=0.)}>
            {React.string("Add Representation Node")}
          </Button>
          <Button onClick={addSchNodeAt(_, ~x=0., ~y=0.)}>
            {React.string("Add Scheme Node")}
          </Button>
          <Button onClick={addDimNodeAt(_, ~x=0., ~y=0.)}>
            {React.string("Add Dimension Node")}
          </Button>
          <Button onClick={addTokNodeAt(_, ~x=0., ~y=0.)}>
            {React.string("Add Token Node")}
          </Button>
          <Button onClick={duplicateNodes}> {React.string("Duplicate")} </Button>
          <Button.Separator />
          <Button onClick={linkNodes}> {React.string("Connect")} </Button>
          <Button onClick={anchorNodes}> {React.string("Anchor")} </Button>
          <Button.Separator />
          <Button onClick={unlinkNodes}> {React.string("Unlink")} </Button>
          <Button.Separator />
          <Button onClick={deleteNodes}> {React.string("Delete")} </Button>
          <Button.Separator />
          <Button onClick={dump}> {React.string("Dump state")} </Button>
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
            config
            data={state->State.modelState->ModelState.data}
            selection={state->State.modelState->ModelState.selection}
            onSelectionChange={selectionChange}
            onNodePositionChange={movedNodes}
            keybindings={keybindings}
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
