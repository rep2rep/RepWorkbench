module App = {
  type state = State.t
  type action = Event.t
  // | GlobalAction(Action.t)
  // | ModelAction(Uuid.t, Action.Model.t)
  // | GraphAction(Uuid.t, ModelAction.t)
  // | InspectorAction(Uuid.t, InspectorAction.t)

  let init = State.load()->Option.getWithDefault(State.empty)
  let reducer = (state, action) => {
    // let newState = switch action {
    // | GlobalAction(action) => Action.dispatch(state, action)
    // | ModelAction(focused, action) =>
    //   state
    //   ->State.model(focused)
    //   ->Option.map(model => {
    //     let model' = Action.Model.dispatch(model, action)
    //     state->State.updateModel(focused, model')
    //   })
    //   ->Option.getWithDefault(state)
    // // | GraphAction(focused, action) =>
    //   state
    //   ->State.model(focused)
    //   ->Option.map(model => {
    //     let modelState = model->State.Model.modelState->ModelAction.dispatch(action)
    //     let model' = model->State.Model.updateModelState(modelState)
    //     state->State.updateModel(focused, model')
    //   })
    //   ->Option.getWithDefault(state)
    // | InspectorAction(focused, action) =>
    //   state
    //   ->State.model(focused)
    //   ->Option.map(model => {
    //     let slots = model->State.Model.slots
    //     let slots' = slots->Uuid.Map.mapWithKey((id, s) =>
    //       switch InspectorAction.dispatch(InspectorState.Single(id, s), action) {
    //       | InspectorState.Single(_, s') => s'
    //       | _ => raise(Not_found)
    //       }
    //     )
    //     let info = model->State.Model.info
    //     let info' = switch InspectorAction.dispatch(InspectorState.Global(info), action) {
    //     | InspectorState.Global(s') => s'
    //     | _ => raise(Not_found)
    //     }

    //     let model' = model->State.Model.updateSlots(slots')->State.Model.updateInfo(info')
    //     state->State.updateModel(focused, model')
    //   })
    //   ->Option.getWithDefault(state)
    // }
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

    // let dispatchA = a => dispatch(GlobalAction(a))
    // let dispatchM = a => focused->Option.iter(focused => dispatch(ModelAction(focused, a)))
    // let dispatchG = a => focused->Option.iter(focused => dispatch(GraphAction(focused, a)))
    // let dispatchI = a => focused->Option.iter(focused => dispatch(InspectorAction(focused, a)))

    let newModel = () => dispatch(Event.File.NewModel(Uuid.create())->Event.File)
    let deleteModel = id => dispatch(Event.File.DeleteModel(id)->Event.File)
    let focusModel = id => dispatch(Event.File.FocusModel(id)->Event.File)
    let reorder = newOrder => dispatch(Event.File.ReorderModels(newOrder)->Event.File)
    // let renameModel = (id, name) => {
    //   dispatch(
    //     InspectorAction(
    //       id,
    //       InspectorAction.UpdateGlobal(InspectorEvent.Model(InspectorEvent.Model.Name(name))),
    //     ),
    //   )
    // }
    let renameModel = (id, name) => dispatch(Event.Model(id, Event.Model.Rename(name)))
    let duplicateModel = id => {
      let newId = Uuid.create()
      // dispatchA(Action.DuplicateModel(id, newId))
      dispatch(Event.File.DuplicateModel(id, newId)->Event.File)
    }
    let selectionChange = (~oldSelection as _, ~newSelection) =>
      // dispatchG(ModelAction.Selection(newSelection))
      focused->Option.iter(focused =>
        dispatch(Event.Model(focused, Event.Model.Graph(Event.Graph.SetSelection(newSelection))))
      )
    let addNodeAt = (kind, ~x, ~y) =>
      focused->Option.iter(focused => {
        let oldSelection = selection
        let ids = oldSelection->ModelSelection.nodes
        let id = Uuid.create()
        dispatch(Event.Model(focused, Event.Model.CreateNode(id, x, y, kind)))
        // dispatchM(Action.Model.CreateNode(kind, id))
        // dispatchG(ModelAction.Create(x, y, kind, id))
        switch ids {
        | [] => ()
        | _ =>
          ids->Array.forEach(source =>
            // dispatchG(ModelAction.Connect(source, id))
            dispatch(
              Event.Model(
                focused,
                Event.Model.Graph(Event.Graph.LinkNodes(source, id, ModelLink.Kind.Hierarchy)),
              ),
            )
          )
        }
        // selectionChange(~oldSelection, ~newSelection=ModelSelection.ofNodes([id]))
        dispatch(
          Event.Model(
            focused,
            Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.ofNodes([id]))),
          ),
        )
      })
    let addRepNodeAt = (_, ~x, ~y) => ModelNode.Kind.Representation->addNodeAt(~x, ~y)
    let addSchNodeAt = (_, ~x, ~y) => ModelNode.Kind.Scheme->addNodeAt(~x, ~y)
    let addDimNodeAt = (_, ~x, ~y) => ModelNode.Kind.Dimension->addNodeAt(~x, ~y)
    let addTokNodeAt = (_, ~x, ~y) => ModelNode.Kind.Token->addNodeAt(~x, ~y)
    let connectNodes = kind =>
      focused->Option.iter(focused => {
        let ids = selection->ModelSelection.nodes
        switch ids {
        | [source, target] =>
          dispatch(
            Event.Model(focused, Event.Model.Graph(Event.Graph.LinkNodes(source, target, kind))),
          ) // dispatchG(ModelAction.Connect(source, target))
        | _ => ()
        }
      })
    let linkNodes = _ => connectNodes(ModelLink.Kind.Hierarchy)
    // let anchorNodes = _ => {
    //   let ids = selection->ModelSelection.nodes
    //   switch ids {
    //   | [source, target] => dispatchG(ModelAction.Anchor(source, target))
    //   | _ => ()
    //   }
    // }
    let anchorNodes = _ => connectNodes(ModelLink.Kind.Anchor)
    let unlinkNodes = _ =>
      focused->Option.iter(focused => {
        let nodeIds = selection->ModelSelection.nodes
        nodeIds->Array.forEach(source =>
          nodeIds->Array.forEach(target =>
            // dispatchG(ModelAction.Unlink(source, target))
            dispatch(
              Event.Model(focused, Event.Model.Graph(Event.Graph.UnlinkNodes(source, target))),
            )
          )
        )
      })
    let deleteNodes = _ =>
      focused->Option.iter(focused => {
        let oldSelection = selection
        oldSelection
        ->ModelSelection.nodes
        ->Array.forEach(id => {
          // dispatchM(Action.Model.DeleteNode(id))
          // dispatchG(ModelAction.Delete(id))
          dispatch(Event.Model(focused, Event.Model.DeleteNode(id)))
        })
        selectionChange(~oldSelection, ~newSelection=ModelSelection.empty)
      })
    let movedNodes = (nodeId, ~x, ~y) =>
      focused->Option.iter(focused => {
        let nodeId = nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString
        dispatch(Event.Model(focused, Event.Model.Graph(Event.Graph.MoveNode(nodeId, x, y))))
      })
    // dispatchG(ModelAction.Move(nodeId->ReactD3Graph.Node.Id.toString->Uuid.fromString, x, y))
    // let duplicateNodes = _ => {
    //   let oldSelection = selection
    //   let nodeIds = oldSelection->ModelSelection.nodes
    //   Js.Console.log(nodeIds)
    //   if nodeIds != [] {
    //     let nodeMap = nodeIds->Array.map(id => (id, Uuid.create()))->Uuid.Map.fromArray
    //     Js.Console.log(nodeMap)
    //     dispatchG(ModelAction.Duplicate(nodeMap))
    //     dispatchI(InspectorAction.DuplicateSchema(nodeMap))
    //     let newSelection = Uuid.Map.values(nodeMap)->ModelSelection.ofNodes
    //     selectionChange(~oldSelection, ~newSelection)
    //   }
    // }
    let duplicateNodes = _ =>
      focused->Option.iter(focused => {
        let nodeIds = selection->ModelSelection.nodes
        if nodeIds != [] {
          let nodeMap = nodeIds->Array.map(id => (id, Uuid.create()))->Uuid.Map.fromArray
          dispatch(Event.Model(focused, Event.Model.DuplicateNodes(nodeMap)))
          let newSelection = nodeMap->Uuid.Map.values->ModelSelection.ofNodes
          dispatch(Event.Model(focused, Event.Model.Graph(Event.Graph.SetSelection(newSelection))))
        }
      })
    let slotsChange = e =>
      focused->Option.iter(focused => {
        dispatch(Event.Model(focused, e))
        Event.Model.graphEvent(e)->Option.iter(e =>
          dispatch(Event.Model(focused, Event.Model.Graph(e)))
        )
        //   switch nodeId {
        //   | Some(nodeId) => // dispatchI(InspectorAction.UpdateSchema(nodeId, e))
        //     // dispatchG(ModelAction.Update(nodeId, e))
        //     dispatch(Event.Model(fo))
        //   | None => ()
        //   }
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
          <Button onClick={linkNodes} value="Connect" />
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
