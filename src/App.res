module App = {
  module BoolStore = LocalStorage.MakeJsonable(Bool)

  type state = State.t
  type action = Event.t

  let intelligence = Intelligence_Intf.create("worker.js")
  let sendToIntelligence = state => {
    let sentId = switch State.focused(state) {
    | None => {
        let id = Gid.create()
        intelligence->Intelligence_Intf.post({id: id, slots: Gid.Map.empty(), links: []})
        Some(id)
      }
    | Some(focused) =>
      state
      ->State.model(focused)
      ->Option.map(model => {
        let slots = State.Model.slots(model)
        let links =
          State.Model.graph(model)
          ->ModelState.graph
          ->ModelGraph.links
          ->Array.map(link => (
            ModelLink.source(link),
            ModelLink.target(link),
            ModelLink.kind(link),
          ))
        let id = Gid.create()
        intelligence->Intelligence_Intf.post({id: id, slots: slots, links: links})
        id
      })
    }
    state->State.setLastRequestedIntelligence(sentId)
  }

  let init = State.load()->Option.getWithDefault(State.empty)
  let reducer = (state, action) => {
    let newState = Event.dispatch(state, action)
    State.store(newState)
    if Event.shouldTriggerIntelligence(action) {
      sendToIntelligence(newState)
    } else {
      newState
    }
  }

  let config = ReactD3Graph.Config.create(
    ~global=ReactD3Graph.Config.Global.create(
      ~width="100%",
      ~height="calc(100vh - 40px)",
      ~defs=[
        <marker
          id="arrowheadCircle" markerWidth="12" markerHeight="12" refX="6" refY="6" orient="auto">
          <circle cx="6" cy="6" r="6" fill="black" />
        </marker>,
      ],
      (),
    ),
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, init)

    let intelligenceListener = (response: Intelligence_Intf.Response.t) => {
      dispatch(Event.Intelligence.Response(response)->Event.File.Intelligence->Event.File)
    }

    React.useEffect0(() => {
      intelligence->Intelligence_Intf.listen(intelligenceListener)
      dispatch(Event.File(Event.File.Intelligence(Event.Intelligence.Init)))
      None
    })

    let focused = state->State.focused
    let toolbarActive = focused->Option.isSome
    let selection =
      focused
      ->Option.flatMap(focused =>
        state
        ->State.model(focused)
        ->Option.map(model => model->State.Model.graph->ModelState.selection)
      )
      ->Option.getWithDefault(ModelSelection.empty)
    let intel =
      State.latestIntelligence(state)->Option.getWithDefault(Intelligence_Intf.Response.empty)

    let dispatchM = e => focused->Option.iter(focused => dispatch(Event.Model(focused, e)))

    let canUndo =
      focused->Option.map(focused => state->State.canUndo(focused))->Option.getWithDefault(false)
    let canRedo =
      focused->Option.map(focused => state->State.canRedo(focused))->Option.getWithDefault(false)
    let undo = _ => focused->Option.iter(focused => dispatch(Event.File.Undo(focused)->Event.File))
    let redo = _ => focused->Option.iter(focused => dispatch(Event.File.Redo(focused)->Event.File))

    let newModel = () => dispatch(Event.File.NewModel(Gid.create())->Event.File)
    let deleteModel = id => dispatch(Event.File.DeleteModel(id)->Event.File)
    let focusModel = id => dispatch(Event.File.FocusModel(id)->Event.File)
    let reorder = newOrder => dispatch(Event.File.ReorderModels(newOrder)->Event.File)
    let renameModel = (id, name) => dispatch(Event.Model(id, Event.Model.Rename(name)))
    let duplicateModel = id => {
      let newId = Gid.create()
      dispatch(Event.File.DuplicateModel(id, newId)->Event.File)
    }

    let selectionChange = (~oldSelection as _, ~newSelection) =>
      dispatchM(Event.Model.Graph(Event.Graph.SetSelection(newSelection)))
    let addNodeAt = (kind, ~x, ~y) =>
      dispatchM({
        let oldSelection = selection
        let ids = oldSelection->ModelSelection.nodes
        let id = Gid.create()
        let e0 = Event.Model.CreateNode(id, x, y, kind)
        let eLinks = switch ids {
        | [] => []
        | _ =>
          ids->Array.map(source => Event.Model.Graph(
            Event.Graph.LinkNodes(source, id, ModelLink.Kind.Hierarchy),
          ))
        }
        let eFinal = Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.ofNodes([id])))

        Event.Model.Seq(Array.concatMany([[e0], eLinks, [eFinal]]))
      })
    let addRepNodeAt = (_, ~x, ~y) => ModelNode.Kind.Representation->addNodeAt(~x, ~y)
    let addSchNodeAt = (_, ~x, ~y) => ModelNode.Kind.Scheme->addNodeAt(~x, ~y)
    let addDimNodeAt = (_, ~x, ~y) => ModelNode.Kind.Dimension->addNodeAt(~x, ~y)
    let addTokNodeAt = (_, ~x, ~y) => ModelNode.Kind.Token->addNodeAt(~x, ~y)
    let addPlcNodeAt = (_, ~x, ~y) => ModelNode.Kind.Placeholder->addNodeAt(~x, ~y)
    let linkNodes = kind => {
      let ids = selection->ModelSelection.nodes
      switch ids {
      | [source, target] =>
        dispatchM(Event.Model.Graph(Event.Graph.LinkNodes(source, target, kind)))
      | _ => ()
      }
    }
    let connectNodes = _ => linkNodes(ModelLink.Kind.Hierarchy)
    let anchorNodes = _ => linkNodes(ModelLink.Kind.Anchor)
    let relateNodes = _ => linkNodes(ModelLink.Kind.Relation)
    let unlinkNodes = _ => {
      let nodeIds = selection->ModelSelection.nodes
      dispatchM(
        Event.Model.Seq(
          nodeIds->Array.flatMap(source =>
            nodeIds->Array.map(target => Event.Model.Graph(Event.Graph.UnlinkNodes(source, target)))
          ),
        ),
      )
    }
    let deleteNodes = _ => {
      let es = selection->ModelSelection.nodes->Array.map(id => Event.Model.DeleteNode(id))
      let eFinal = Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.empty))

      dispatchM(Event.Model.Seq(Array.concat(es, [eFinal])))
    }
    let movedNodes = (nodeId, ~x, ~y) =>
      focused->Option.iter(focused => {
        let nodeId = nodeId->ReactD3Graph.Node.Id.toString->Gid.fromString
        dispatch(Event.Model(focused, Event.Model.Graph(Event.Graph.MoveNode(nodeId, x, y))))
      })
    let nudge = (_, ~dx, ~dy) => {
      let es =
        selection
        ->ModelSelection.nodes
        ->Array.keepMap(id => {
          let node =
            focused->Option.flatMap(focused =>
              state
              ->State.model(focused)
              ->Option.flatMap(model => model->State.Model.graph->ModelState.nodeWithId(id))
            )
          node->Option.map(node => {
            let (x, y) = node->ModelNode.position
            Event.Model.Graph(Event.Graph.MoveNode(id, x +. dx, y +. dy))
          })
        })
      dispatchM(Event.Model.Seq(es))
    }
    let duplicateNodes = _ => {
      let nodeIds = selection->ModelSelection.nodes
      if nodeIds != [] {
        let nodeMap = nodeIds->Array.map(id => (id, Gid.create()))->Gid.Map.fromArray
        let newSelection = nodeMap->Gid.Map.values->ModelSelection.ofNodes
        dispatchM(
          Event.Model.Seq([
            Event.Model.DuplicateNodes(nodeMap),
            Event.Model.Graph(Event.Graph.SetSelection(newSelection)),
          ]),
        )
      }
    }
    let slotsChange = e => {
      switch Event.Model.graphEvent(e) {
      | None => dispatchM(e)
      | Some(e') => dispatchM(Event.Model.Seq([e, Event.Model.Graph(e')]))
      }
    }
    let clickError = (_, err) => {
      dispatch(
        Event.Intelligence.Focus(ModelError.id(err)->Some)->Event.File.Intelligence->Event.File,
      )
      dispatchM(
        Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.ofNodes(ModelError.nodes(err)))),
      )
    }
    let clickWarning = (_, warn) => {
      dispatch(
        Event.Intelligence.Focus(ModelWarning.id(warn)->Some)->Event.File.Intelligence->Event.File,
      )
      dispatchM(
        Event.Model.Graph(
          Event.Graph.SetSelection(ModelSelection.ofNodes(ModelWarning.nodes(warn))),
        ),
      )
    }
    let deselectErrorOrWarning = _ => {
      dispatch(Event.Intelligence.Focus(None)->Event.File.Intelligence->Event.File)
    }
    let importModel = f => {
      File.text(f)
      |> Js.Promise.then_(text => {
        let model = try text->Js.Json.parseExn->State.Model.Stable.V3.fromJson catch {
        | _ => Or_error.error_s("fail")
        }
        if Or_error.isOk(model) {
          let model = Or_error.okExn(model)
          dispatch(Event.File.ImportModel(model)->Event.File)
        } else {
          Dialog.alert("Failed to import '" ++ File.name(f) ++ "'.")
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
        let json = State.Model.Stable.V3.toJson(model)
        let content =
          "data:text/json;charset=utf-8," ++ json->Js.Json.stringify->Js.Global.encodeURIComponent
        Downloader.download(name ++ ".repn", content)
      })
    }

    module K = GlobalKeybindings.KeyBinding
    GlobalKeybindings.set([
      K.create("Cmd+z", undo),
      K.create("Cmd+Shift+z", redo),
      K.create("Cmd+y", redo),
    ])

    let keybindings = Js.Dict.fromArray([
      ("r", addRepNodeAt),
      ("s", addSchNodeAt),
      ("d", addDimNodeAt),
      ("t", addTokNodeAt),
      ("q", addPlcNodeAt),
      ("c", (e, ~x as _, ~y as _) => connectNodes(e)),
      ("a", (e, ~x as _, ~y as _) => anchorNodes(e)),
      ("e", (e, ~x as _, ~y as _) => relateNodes(e)),
      ("x", (e, ~x as _, ~y as _) => deleteNodes(e)),
      ("ArrowLeft", (e, ~x as _, ~y as _) => nudge(e, ~dx=-10.0, ~dy=0.0)),
      ("ArrowRight", (e, ~x as _, ~y as _) => nudge(e, ~dx=10.0, ~dy=0.0)),
      ("ArrowUp", (e, ~x as _, ~y as _) => nudge(e, ~dx=0.0, ~dy=-10.0)),
      ("ArrowDown", (e, ~x as _, ~y as _) => nudge(e, ~dx=0.0, ~dy=10.0)),
      ("Shift+ArrowLeft", (e, ~x as _, ~y as _) => nudge(e, ~dx=-1.0, ~dy=0.0)),
      ("Shift+ArrowRight", (e, ~x as _, ~y as _) => nudge(e, ~dx=1.0, ~dy=0.0)),
      ("Shift+ArrowUp", (e, ~x as _, ~y as _) => nudge(e, ~dx=0.0, ~dy=-1.0)),
      ("Shift+ArrowDown", (e, ~x as _, ~y as _) => nudge(e, ~dx=0.0, ~dy=1.0)),
      ("Backspace", (e, ~x as _, ~y as _) => deleteNodes(e)),
      ("Delete", (e, ~x as _, ~y as _) => deleteNodes(e)),
      ("v", (e, ~x as _, ~y as _) => unlinkNodes(e)),
      ("Ctrl+d", (e, ~x as _, ~y as _) => duplicateNodes(e)),
    ])

    let (showGrid, setShowGrid) = React.useState(_ => {
      BoolStore.get("REP-SHOW-GRID")->Or_error.getWithDefault(false)
    })

    let toggleGrid = _ => {
      if showGrid {
        BoolStore.set("REP-SHOW-GRID", false)
        setShowGrid(_ => false)
      } else {
        BoolStore.set("REP-SHOW-GRID", true)
        setShowGrid(_ => true)
      }
    }

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
            ~height="30px",
            ~borderBottom="1px solid black",
            ~padding="0 0.5rem",
            (),
          )}>
          <Button onClick={undo} value="Undo" enabled={canUndo} tooltip="Cmd+Z" />
          <Button onClick={redo} value="Redo" enabled={canRedo} tooltip="Cmd+Shift+Z" />
          <Button.Separator />
          <Button
            onClick={addRepNodeAt(_, ~x=0., ~y=0.)}
            value="Representation"
            enabled={toolbarActive}
            tooltip="R"
          />
          <Button
            onClick={addSchNodeAt(_, ~x=0., ~y=0.)}
            value="Scheme"
            enabled={toolbarActive}
            tooltip="S"
          />
          <Button
            onClick={addDimNodeAt(_, ~x=0., ~y=0.)}
            value="Dimension"
            enabled={toolbarActive}
            tooltip="D"
          />
          <Button
            onClick={addTokNodeAt(_, ~x=0., ~y=0.)}
            value="Token"
            enabled={toolbarActive}
            tooltip="T"
          />
          <Button
            onClick={addPlcNodeAt(_, ~x=0., ~y=0.)}
            value="Placeholder"
            enabled={toolbarActive}
            tooltip="Q"
          />
          <Button.Separator />
          <Button
            onClick={duplicateNodes} value="Duplicate" enabled={toolbarActive} tooltip="Ctrl+D"
          />
          <Button.Separator />
          <Button onClick={connectNodes} value="Connect" enabled={toolbarActive} tooltip="C" />
          <Button onClick={anchorNodes} value="Anchor" enabled={toolbarActive} tooltip="A" />
          <Button onClick={relateNodes} value="Relate" enabled={toolbarActive} tooltip="E" />
          <Button.Separator />
          <Button onClick={unlinkNodes} value="Unlink" enabled={toolbarActive} tooltip="V" />
          <Button.Separator />
          <Button onClick={deleteNodes} value="Delete" enabled={toolbarActive} tooltip="X" />
          <Button.Separator />
          <label htmlFor="gridToggle"> {React.string("Grid")} </label>
          <input
            type_="checkbox"
            label="gridToggle"
            onChange={toggleGrid}
            disabled={!toolbarActive}
            checked={showGrid}
            style={ReactDOM.Style.make(~marginLeft="0.5em", ())}
          />
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
          <div
            style={ReactDOM.Style.make(
              ~flexGrow="1",
              ~display="flex",
              ~flexDirection="column",
              ~position="relative",
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
              showGrid
              style={ReactDOM.Style.make(~flexGrow="1", ())}
            />
            <IntelligenceUI
              errors={intel.errors}
              warnings={intel.warnings}
              selected={State.focusedErrorOrWarning(state)}
              isUpToDate={State.intelligenceIsUpToDate(state)}
              onClickError={clickError}
              onClickWarning={clickWarning}
              onDeselect={deselectErrorOrWarning}
            />
          </div>
          <InspectorPanel
            id={"node_inspector"}
            onChange=slotsChange
            data={focused
            ->Option.flatMap(focused =>
              state
              ->State.model(focused)
              ->Option.map(model => {
                let slots = model->State.Model.slotsForSelection(selection)->Gid.Map.toArray
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
