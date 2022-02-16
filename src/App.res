module App = {
  type state = State.t
  type action = Event.t

  let init = State.load()->Option.getWithDefault(State.empty)
  let reducer = (state, action) => {
    let newState = Event.dispatch(state, action)
    State.store(newState)
    newState
  }

  @val external alert: string => unit = "alert"

  let config = ReactD3Graph.Config.create(
    ~global=ReactD3Graph.Config.Global.create(~width="100%", ~height="calc(100vh - 40px)", ()),
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)
    let focused = state->State.focused
    let selection =
      focused
      ->Option.flatMap(focused =>
        state
        ->State.model(focused)
        ->Option.map(model => model->State.Model.graph->ModelState.selection)
      )
      ->Option.getWithDefault(ModelSelection.empty)

    let dispatchM = e => focused->Option.iter(focused => dispatch(Event.Model(focused, e)))

    let newModel = () => dispatch(Event.File.NewModel(Uuid.create())->Event.File)
    let deleteModel = id => dispatch(Event.File.DeleteModel(id)->Event.File)
    let focusModel = id => dispatch(Event.File.FocusModel(id)->Event.File)
    let reorder = newOrder => dispatch(Event.File.ReorderModels(newOrder)->Event.File)
    let renameModel = (id, name) => dispatch(Event.Model(id, Event.Model.Rename(name)))
    let duplicateModel = id => {
      let newId = Uuid.create()
      dispatch(Event.File.DuplicateModel(id, newId)->Event.File)
    }

    let selectionChange = (~oldSelection as _, ~newSelection) =>
      dispatchM(Event.Model.Graph(Event.Graph.SetSelection(newSelection)))
    let addNodeAt = (kind, ~x, ~y) =>
      focused->Option.iter(focused => {
        let oldSelection = selection
        let ids = oldSelection->ModelSelection.nodes
        let id = Uuid.create()
        let e0 = Event.Model(focused, Event.Model.CreateNode(id, x, y, kind))
        let eLinks = switch ids {
        | [] => []
        | _ =>
          ids->Array.map(source => Event.Model(
            focused,
            Event.Model.Graph(Event.Graph.LinkNodes(source, id, ModelLink.Kind.Hierarchy)),
          ))
        }
        let eFinal = Event.Model(
          focused,
          Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.ofNodes([id]))),
        )
        dispatch(Event.Seq(Array.concatMany([[e0], eLinks, [eFinal]])))
      })
    let addRepNodeAt = (_, ~x, ~y) => ModelNode.Kind.Representation->addNodeAt(~x, ~y)
    let addSchNodeAt = (_, ~x, ~y) => ModelNode.Kind.Scheme->addNodeAt(~x, ~y)
    let addDimNodeAt = (_, ~x, ~y) => ModelNode.Kind.Dimension->addNodeAt(~x, ~y)
    let addTokNodeAt = (_, ~x, ~y) => ModelNode.Kind.Token->addNodeAt(~x, ~y)
    let linkNodes = kind => {
      let ids = selection->ModelSelection.nodes
      switch ids {
      | [source, target] =>
        dispatchM(Event.Model.Graph(Event.Graph.LinkNodes(source, target, kind)))
      | _ => ()
      }
    }
    //)
    let connectNodes = _ => linkNodes(ModelLink.Kind.Hierarchy)
    let anchorNodes = _ => linkNodes(ModelLink.Kind.Anchor)
    let unlinkNodes = _ =>
      focused->Option.iter(focused => {
        let nodeIds = selection->ModelSelection.nodes
        dispatch(
          Event.Seq(
            nodeIds->Array.flatMap(source =>
              nodeIds->Array.map(target => Event.Model(
                focused,
                Event.Model.Graph(Event.Graph.UnlinkNodes(source, target)),
              ))
            ),
          ),
        )
      })
    let deleteNodes = _ =>
      focused->Option.iter(focused => {
        let es =
          selection
          ->ModelSelection.nodes
          ->Array.map(id => {
            Event.Model(focused, Event.Model.DeleteNode(id))
          })
        let eFinal = Event.Model(
          focused,
          Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.empty)),
        )
        dispatch(Event.Seq(Array.concat(es, [eFinal])))
      })
    let movedNodes = (nodeId, ~x, ~y) =>
      focused->Option.iter(focused => {
        let nodeId = nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString
        dispatch(Event.Model(focused, Event.Model.Graph(Event.Graph.MoveNode(nodeId, x, y))))
      })
    let duplicateNodes = _ =>
      focused->Option.iter(focused => {
        let nodeIds = selection->ModelSelection.nodes
        if nodeIds != [] {
          let nodeMap = nodeIds->Array.map(id => (id, Uuid.create()))->Uuid.Map.fromArray
          let newSelection = nodeMap->Uuid.Map.values->ModelSelection.ofNodes
          dispatch(
            Event.Seq([
              Event.Model(focused, Event.Model.DuplicateNodes(nodeMap)),
              Event.Model(focused, Event.Model.Graph(Event.Graph.SetSelection(newSelection))),
            ]),
          )
        }
      })
    let slotsChange = e =>
      focused->Option.iter(focused => {
        dispatch(Event.Model(focused, e))
        Event.Model.graphEvent(e)->Option.iter(e =>
          dispatch(Event.Model(focused, Event.Model.Graph(e)))
        )
      })
    let importModel = f => {
      File.text(f)
      |> Js.Promise.then_(text => {
        let model = try text->Js.Json.parseExn->State.Model.Stable.V2.fromJson catch {
        | _ => Or_error.error_s("fail")
        }
        if Or_error.isOk(model) {
          let model = Or_error.okExn(model)
          dispatch(Event.File.ImportModel(model)->Event.File)
        } else {
          alert("Failed to import '" ++ File.name(f) ++ "'.")
        }
        Js.Promise.resolve()
      })
      |> ignore
    }
    let exportModel = id => {
      state
      ->State.model(id)
      ->Option.iter(model => {
        let name = State.Model.info(model).name
        let json = State.Model.Stable.V2.toJson(model)
        let content =
          "data:text/json;charset=utf-8," ++ json->Js.Json.stringify->Js.Global.encodeURIComponent
        Downloader.download(name ++ ".repn", content)
      })
    }

    let keybindings = Js.Dict.fromArray([
      ("r", addRepNodeAt),
      ("s", addSchNodeAt),
      ("d", addDimNodeAt),
      ("t", addTokNodeAt),
      ("c", (e, ~x as _, ~y as _) => connectNodes(e)),
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
        active={State.focused(state)}
        onCreate={newModel}
        onDelete={deleteModel}
        onSelect={focusModel}
        onDuplicate={duplicateModel}
        onChangedName={renameModel}
        onReorder={reorder}
        onImport={importModel}
        onExport={exportModel}
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
          <Button onClick={addRepNodeAt(_, ~x=0., ~y=0.)} value="Add Representation Node" />
          <Button onClick={addSchNodeAt(_, ~x=0., ~y=0.)} value="Add Scheme Node" />
          <Button onClick={addDimNodeAt(_, ~x=0., ~y=0.)} value="Add Dimension Node" />
          <Button onClick={addTokNodeAt(_, ~x=0., ~y=0.)} value="Add Token Node" />
          <Button onClick={duplicateNodes} value="Duplicate" />
          <Button.Separator />
          <Button onClick={connectNodes} value="Connect" />
          <Button onClick={anchorNodes} value="Anchor" />
          <Button.Separator />
          <Button onClick={unlinkNodes} value="Unlink" />
          <Button.Separator />
          <Button onClick={deleteNodes} value="Delete" />
          <Button.Separator />
          <a href="manual.html" target="_blank"> {React.string("Manual")} </a>
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
            data={focused
            ->Option.flatMap(focused =>
              state
              ->State.model(focused)
              ->Option.map(model => model->State.Model.graph->ModelState.data)
            )
            ->Option.getWithDefault(ModelState.empty->ModelState.data)}
            selection
            onSelectionChange={selectionChange}
            onNodePositionChange={movedNodes}
            keybindings={keybindings}
            style={ReactDOM.Style.make(~flexGrow="1", ())}
          />
          // <InspectorPanel
          //   id={"node-inspector"}
          //   data={switch selection->ModelSelection.nodes {
          //   | [] =>
          //     switch focused {
          //     | None => InspectorState.Empty
          //     | Some(focused) =>
          //       state
          //       ->State.model(focused)
          //       ->Option.map(model => model->State.Model.info->InspectorState.Global)
          //       ->Option.getWithDefault(InspectorState.Empty)
          //     }
          //   | [nodeId] =>
          //     focused
          //     ->Option.flatMap(focused =>
          //       state
          //       ->State.model(focused)
          //       ->Option.flatMap(model =>
          //         model
          //         ->State.Model.slots
          //         ->Uuid.Map.get(nodeId)
          //         ->Option.map(s => InspectorState.Single(nodeId, s))
          //       )
          //     )
          //     ->Option.getWithDefault(InspectorState.Empty)
          //   | nodeIds =>
          //     focused
          //     ->Option.flatMap(focused =>
          //       state
          //       ->State.model(focused)
          //       ->Option.map(model => {
          //         let slots = model->State.Model.slots
          //         nodeIds
          //         ->Array.mapPartial(id => slots->Uuid.Map.get(id)->Option.map(s => (id, s)))
          //         ->InspectorState.Multiple
          //       })
          //     )
          //     ->Option.getWithDefault(InspectorState.Empty)
          //   }}
          //   onChange=slotsChange
          // />
          <InspectorPanel
            id={"node_inspector"}
            onChange=slotsChange
            data={focused
            ->Option.flatMap(focused =>
              state
              ->State.model(focused)
              ->Option.map(model => {
                let slots = model->State.Model.slotsForSelection(selection)->Uuid.Map.toArray
                switch slots {
                | [] => InspectorState.Global(State.Model.info(model))
                | [(id, slot)] => InspectorState.Single(id, slot)
                | multi => InspectorState.Multiple(multi)
                }
              })
            )
            ->Option.getWithDefault(InspectorState.Empty)}
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
