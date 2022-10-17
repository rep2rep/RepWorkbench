type performance
@val external performance: performance = "performance"
@send external perfNow: performance => float = "now"

type window
@val external window: window = "window"

module App = {
  module BoolStore = LocalStorage.MakeJsonable(Bool)
  module K = GlobalKeybindings.KeyBinding
  module FP = {
    include FilePanel
    let make = React.memo(make)
  }
  module Graph = {
    include ReactD3Graph.Graph
    let make = React.memo(make)
  }

  type state = State.t
  type action = Event.t

  let old = ref(None)
  let ifChanged = (f, v) => {
    switch old.contents {
    | None => {
        old := Some(v)
        f()
      }
    | Some(v') =>
      if v !== v' {
        old := Some(v)
        f()
      }
    }
  }

  let intelligence = IntelligencePool.create("worker.js?v=##VERSION##")
  let intelTimeout = ref(None)
  let sendToIntelligence = state => {
    intelTimeout.contents->Option.iter(Js.Global.clearTimeout)
    state
    ->State.focused
    ->Option.map(focused =>
      state->State.updateModelBypassUndoRedo(focused, model => {
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

        intelTimeout := Js.Global.setTimeout(() =>
            intelligence->IntelligencePool.post({
              id: id,
              model: focused,
              slots: slots->Gid.Map.mapPartial((_, slot) =>
                switch slot {
                | InspectorState.SchemaOrLink.Schema(s) => Some(s)
                | _ => None
                }
              ),
              links: links,
            })
          , 500)->Some
        model->State.Model.setRequestedIntelligence(Some(id))
      })
    )
    ->Option.getWithDefault(state)
  }

  let db_store = "RISN"
  let db_ready = IndexedDB.open_(~name="risn", ~version=1, ~onUpgradeNeeded=db =>
    db->IndexedDB.createObjectStore(db_store)
  )->Promise.thenResolve(db => {
    db->IndexedDB.onError(e => {
      Js.Console.log(e)
      Dialog.alert("Database Error!")
    })
    State.setDB(db, db_store)
  })
  let init =
    db_ready
    ->Promise.then(_ => State.load(~atTime=perfNow(performance)))
    ->Promise.thenResolve(s => s->Option.getWithDefault(State.empty))
  let reducer = (state, action) => {
    let atTime = perfNow(performance)
    if Recording.isRecording() {
      Recording.record(action, ~atTime)
    }
    let newState = Event.dispatch(state, action, ~atTime)
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
          id="arrowheadCircle" markerWidth="12" markerHeight="12" refX="-6" refY="6" orient="auto">
          <circle cx="6" cy="6" r="6" fill="black" />
        </marker>,
        <marker
          id="arrowheadCircleSmall"
          markerWidth="6"
          markerHeight="6"
          refX="3"
          refY="3"
          orient="auto">
          <circle
            cx="3" cy="3" r="2" fill="white" stroke="black" strokeWidth="1" strokeDasharray="1000"
          />
        </marker>,
        <marker
          id="arrowheadDiamond" markerWidth="6" markerHeight="6" refX="3" refY="3" orient="auto">
          <polygon points="3,0 6,3 3,6 0,3" fill="black" />
        </marker>,
      ],
      (),
    ),
    ~link=ReactD3Graph.Link.Config.create(
      ~color=ReactD3Graph.Color.ofHexString("#000000"),
      ~renderLabel=true,
      ~fontSize=10.0,
      ~fontScaling=false,
      ~strokeWidth=1.,
      ~markerEnd="arrowhead",
      (),
    ),
    ~d3=ReactD3Graph.Config.D3.create(~disableLinkForce=true, ()),
    (),
  )

  @react.component
  let make = (~init) => {
    let (state, dispatch) = React.useReducer(reducer, init)

    let (isRecording, setIsRecording_) = React.useState(_ => false)
    let setIsRecording = b => {
      if b && !isRecording {
        Recording.startRecording(state, ~atTime=perfNow(performance))
        setIsRecording_(_ => b)
      } else {
        let result = Recording.stopRecording()
        let json = Recording.toJson(result)
        let name = {
          let date = Js.Date.make()
          date->Js.Date.toLocaleDateString ++ " " ++ date->Js.Date.toLocaleTimeString
        }
        let zip = Zip.create()
        zip->Zip.root->Zip.Folder.createFile("0", json->Js.Json.stringify)
        zip
        ->Zip.generateAsync
        ->Promise.thenResolve(base64 => {
          let content = "data:application/zip;base64," ++ base64
          Downloader.download(name ++ ".risnrec", content)
        })
        ->ignore
        setIsRecording_(_ => b)
      }
    }

    let intelligenceListener = React.useCallback1((response: Intelligence_Intf.Response.t) => {
      dispatch(Event.Intelligence.Response(response)->Event.File.Intelligence->Event.File)
    }, [dispatch])

    React.useEffect0(() => {
      intelligence->IntelligencePool.listen(intelligenceListener)
      dispatch(Event.File(Event.File.Intelligence(Event.Intelligence.Init)))
      None
    })

    let stateHash = State.hash(state)

    let focused = React.useMemo1(
      () =>
        state
        ->State.focused
        ->Option.flatMap(id => state->State.model(id))
        ->Option.flatMap(_ => state->State.focused),
      [state->State.focused],
    )
    let toolbarActive = React.useMemo1(() => focused->Option.isSome, [focused])
    let selection = React.useMemo2(
      () =>
        focused
        ->Option.flatMap(focused =>
          state
          ->State.model(focused)
          ->Option.map(model => model->State.Model.graph->ModelState.selection)
        )
        ->Option.getWithDefault(ModelSelection.empty),
      (focused, state),
    )
    let intel =
      focused
      ->Option.flatMap(focused =>
        state->State.model(focused)->Option.flatMap(model => model->State.Model.intelligence)
      )
      ->Option.getWithDefault(Intelligence_Intf.Response.empty)

    let dispatchM = React.useCallback2(
      e => focused->Option.iter(focused => dispatch(Event.Model(focused, e))),
      (focused, dispatch),
    )

    // Node Inline Editing
    React.useEffect1(() => {
      ModelNodeEdit.setGlobal((id, schema, slot, value) => {
        let e = switch schema {
        | "Representation" =>
          switch slot {
          | "Domain" => Some(Event.Slots.Representation.Domain(value))
          | "Display" => Some(Event.Slots.Representation.Display(value))
          | _ => None
          }->Option.map(e => Event.Slots.Representation(e))
        | "Scheme" =>
          switch slot {
          | "Concept" => Some(Event.Slots.Scheme.Concept_structure(value))
          | "Graphic" => Some(Event.Slots.Scheme.Graphic_structure(value))
          | _ => None
          }->Option.map(e => Event.Slots.Scheme(e))
        | "Dimension" =>
          switch slot {
          | "Concept" => Some(Event.Slots.Dimension.Concept(value))
          | "Graphic" => Some(Event.Slots.Dimension.Graphic(value))
          | _ => None
          }->Option.map(e => Event.Slots.Dimension(e))
        | "Token" =>
          switch slot {
          | "Concept" => Some(Event.Slots.Token.Concept(value))
          | "Graphic" => Some(Event.Slots.Token.Graphic(value))
          | _ => None
          }->Option.map(e => Event.Slots.Token(e))
        | "Placeholder" =>
          switch slot {
          | "Description" =>
            Some(Event.Slots.Placeholder(Event.Slots.Placeholder.Description(value)))
          | _ => None
          }

        | _ => None
        }
        let e = e->Option.map(e => Event.Model.Slots(id, e))
        switch e {
        | Some(e) =>
          switch Event.Model.graphEvent(e) {
          | None => dispatchM(e)
          | Some(e') => dispatchM(Event.Model.Seq([e, Event.Model.Graph(e')]))
          }
        | None => Js.Console.log((id, schema, slot, value))
        }
      })
      None
    }, [dispatchM])

    let canUndo =
      focused->Option.map(focused => state->State.canUndo(focused))->Option.getWithDefault(false)
    let canRedo =
      focused->Option.map(focused => state->State.canRedo(focused))->Option.getWithDefault(false)
    let undo = React.useCallback2(
      _ => focused->Option.iter(focused => dispatch(Event.File.Undo(focused)->Event.File)),
      (focused, dispatch),
    )
    let redo = React.useCallback2(
      _ => focused->Option.iter(focused => dispatch(Event.File.Redo(focused)->Event.File)),
      (focused, dispatch),
    )

    let newModel = React.useCallback1(
      path => dispatch(Event.File.NewModel(Gid.create(), path)->Event.File),
      [dispatch],
    )
    let newFolder = React.useCallback1(
      path => dispatch(Event.File.NewFolder(Gid.create(), path)->Event.File),
      [dispatch],
    )
    let deleteModel = React.useCallback1(
      id => dispatch(Event.File.DeleteModel(id)->Event.File),
      [dispatch],
    )
    let deleteFolder = React.useCallback1(
      id => dispatch(Event.File.DeleteFolder(id)->Event.File),
      [dispatch],
    )
    let focusModel = React.useCallback1(
      id => dispatch(Event.File.FocusModel(id)->Event.File),
      [dispatch],
    )
    let reorder = React.useCallback1(
      newOrder => dispatch(Event.File.ReorderModels(newOrder)->Event.File),
      [dispatch],
    )
    let renameModel = React.useCallback1(
      (id, name) => dispatch(Event.Model(id, Event.Model.Rename(name))),
      [dispatch],
    )
    let renameFolder = React.useCallback1(
      (id, name) => dispatch(Event.File.RenameFolder(id, name)->Event.File),
      [dispatch],
    )
    let duplicateModel = React.useCallback1(id => {
      let newId = Gid.create()
      dispatch(Event.File.DuplicateModel(id, newId)->Event.File)
    }, [dispatch])

    let selectionChange = React.useMemo1(((), ~oldSelection as _, ~newSelection) => {
      dispatchM(Event.Model.Graph(Event.Graph.SetSelection(newSelection)))
      ModelNodeEdit.callLocal(newSelection)
    }, [dispatchM])
    let addNodeAt = React.useMemo2(((), kind, ~x, ~y) =>
      dispatchM({
        let oldSelection = selection
        let ids = oldSelection->ModelSelection.nodes
        let id = Gid.create()
        let e0 = Event.Model.CreateNode(id, x, y, kind)
        let eLinks = switch ids {
        | [] => []
        | _ =>
          ids->Array.map(source => Event.Model.LinkNodes({
            linkId: Gid.create(),
            source: source,
            target: id,
            kind: ModelLink.Kind.Hierarchy,
            label: None,
          }))
        }
        let newSelection = ModelSelection.ofNodes([id])
        let eFinal = Event.Model.Graph(Event.Graph.SetSelection(newSelection))
        ModelNodeEdit.callLocal(newSelection)
        Event.Model.Seq(Array.concatMany([[e0], eLinks, [eFinal]]))
      })
    , (dispatchM, selection))
    let addRepNodeAt = React.useMemo1(
      ((), ~x, ~y) => ModelNode.Kind.Representation->addNodeAt(~x, ~y),
      [addNodeAt],
    )
    let addSchNodeAt = React.useMemo1(
      ((), ~x, ~y) => ModelNode.Kind.Scheme->addNodeAt(~x, ~y),
      [addNodeAt],
    )
    let addDimNodeAt = React.useMemo1(
      ((), ~x, ~y) => ModelNode.Kind.Dimension->addNodeAt(~x, ~y),
      [addNodeAt],
    )
    let addTokNodeAt = React.useMemo1(
      ((), ~x, ~y) => ModelNode.Kind.Token->addNodeAt(~x, ~y),
      [addNodeAt],
    )
    let addPlcNodeAt = React.useMemo1(
      ((), ~x, ~y) => ModelNode.Kind.Placeholder->addNodeAt(~x, ~y),
      [addNodeAt],
    )
    let linkNodes = React.useMemo2(((), kind, ~reversed) => {
      let ids = selection->ModelSelection.nodes
      switch ids {
      | [] => ()
      | [source, target] =>
        if reversed {
          dispatchM(
            Event.Model.LinkNodes({
              linkId: Gid.create(),
              source: target,
              target: source,
              kind: kind,
              label: None,
            }),
          )
        } else {
          dispatchM(
            Event.Model.LinkNodes({
              linkId: Gid.create(),
              source: source,
              target: target,
              kind: kind,
              label: None,
            }),
          )
        }
      | many => {
          let source = many[0]->Option.getExn
          let targets = many->Js.Array2.sliceFrom(1)
          targets
          ->Array.map(target =>
            if reversed {
              Event.Model.LinkNodes({
                linkId: Gid.create(),
                source: target,
                target: source,
                kind: kind,
                label: None,
              })
            } else {
              Event.Model.LinkNodes({
                linkId: Gid.create(),
                source: source,
                target: target,
                kind: kind,
                label: None,
              })
            }
          )
          ->Event.Model.Seq
          ->dispatchM
        }
      }
    }, (selection, dispatchM))
    let connectNodes = React.useCallback1(
      reversed => linkNodes(ModelLink.Kind.Hierarchy, ~reversed),
      [linkNodes],
    )
    let anchorNodes = React.useCallback1(
      reversed => linkNodes(ModelLink.Kind.Anchor, ~reversed),
      [linkNodes],
    )
    let makeGenericLink = React.useCallback1(
      _ => linkNodes(ModelLink.Kind.Generic, ~reversed=false),
      [linkNodes],
    )
    let delete = React.useCallback2(_ => {
      let nodes = selection->ModelSelection.nodes->Array.map(id => Event.Model.DeleteNode(id))
      let links = selection->ModelSelection.links->Array.map(id => Event.Model.DeleteLink(id))
      let clearSelection = Event.Model.Graph(Event.Graph.SetSelection(ModelSelection.empty))
      dispatchM(Event.Model.Seq(Array.concatMany([nodes, links, [clearSelection]])))
      ModelNodeEdit.callLocal(ModelSelection.empty)
    }, (selection, dispatchM))
    let unlinkNodes = React.useCallback4(_ => {
      let edges =
        focused
        ->Option.flatMap(focused =>
          state
          ->State.model(focused)
          ->Option.map(model =>
            model
            ->State.Model.graph
            ->ModelState.linksConnectingNodes(selection->ModelSelection.nodes)
          )
        )
        ->Option.getWithDefault([])
      let events = edges->Array.map(id => Event.Model.DeleteLink(id))
      dispatchM(Event.Model.Seq(events))
    }, (focused, dispatchM, selection, stateHash))
    let movedNodes = React.useMemo1(((), nodeId, ~x, ~y) => {
      let nodeId = nodeId->ReactD3Graph.Node.Id.toString->Gid.fromString
      dispatchM(Event.Model.Graph(Event.Graph.MoveNode(nodeId, x, y)))
    }, [dispatchM])
    let nudge = React.useMemo3(((), ~dx, ~dy) => {
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
    }, (selection, dispatchM, stateHash))
    let duplicateNodes = React.useCallback3(_ => {
      let nodeIds = selection->ModelSelection.nodes
      let linkIds =
        focused
        ->Option.flatMap(focused =>
          state
          ->State.model(focused)
          ->Option.map(model =>
            model
            ->State.Model.graph
            ->ModelState.linksConnectingNodes(selection->ModelSelection.nodes)
          )
        )
        ->Option.getWithDefault([])

      let allIds = Array.concat(nodeIds, linkIds)
      if allIds != [] {
        let idMap = allIds->Array.map(id => (id, Gid.create()))->Gid.Map.fromArray
        let newSelection =
          nodeIds->Array.mapPartial(id => idMap->Gid.Map.get(id))->ModelSelection.ofNodes
        dispatchM(
          Event.Model.Seq([
            Event.Model.Duplicate(idMap),
            Event.Model.Graph(Event.Graph.SetSelection(newSelection)),
          ]),
        )
        ModelNodeEdit.callLocal(newSelection)
      }
    }, (selection, dispatchM, stateHash))
    let slotsChange = React.useCallback1(e => {
      switch Event.Model.graphEvent(e) {
      | None => dispatchM(e)
      | Some(e') => dispatchM(Event.Model.Seq([e, Event.Model.Graph(e')]))
      }
    }, [dispatchM])
    let clickIntel = React.useCallback2((id, nodes) => {
      let newSelection = ModelSelection.ofNodes(nodes)
      focused->Option.iter(m =>
        dispatch(Event.Intelligence.Focus(m, Some(id))->Event.File.Intelligence->Event.File)
      )
      dispatchM(Event.Model.Graph(Event.Graph.SetSelection(newSelection)))
      ModelNodeEdit.callLocal(newSelection)
    }, (focused, dispatchM))
    let clickError = React.useCallback1(
      (_, err) => clickIntel(ModelError.id(err), ModelError.nodes(err)),
      [clickIntel],
    )
    let clickWarning = React.useCallback1(
      (_, err) => clickIntel(ModelWarning.id(err), ModelWarning.nodes(err)),
      [clickIntel],
    )
    let clickInsight = React.useCallback1(
      (_, err) => clickIntel(ModelInsight.id(err), ModelInsight.nodes(err)),
      [clickIntel],
    )
    let deselectErrorOrWarning = React.useCallback2(_ => {
      focused->Option.iter(m =>
        dispatch(Event.Intelligence.Focus(m, None)->Event.File.Intelligence->Event.File)
      )
    }, (focused, dispatch))

    let onZoomed = React.useMemo2(
      ((), ~oldZoom as _, ~newZoom) =>
        focused->Option.iter(id => dispatch(Event.File.ViewTransform(id, newZoom)->Event.File)),
      (focused, dispatch),
    )

    let importModels = React.useCallback1((fs, path) =>
      fs->Array.forEach(f => {
        File.text(f)
        |> Js.Promise.then_(text => {
          let model = try text->Js.Json.parseExn->State.Model.Stable.V5.fromJson catch {
          | _ => Or_error.error_s("fail")
          }
          switch model->Or_error.match {
          | Or_error.Ok(model) => {
              let model =
                model->State.Model.addToplevelNote(
                  "\n*** Imported " ++ Js.Date.make()->Js.Date.toString ++ " ***",
                )
              dispatch(Event.File.ImportModel(Gid.create(), model, path)->Event.File)
            }
          | Or_error.Err(e) => {
              Js.Console.log(e)
              Dialog.alert("Failed to import '" ++ File.name(f) ++ "'.")
            }
          }
          Js.Promise.resolve()
        })
        |> ignore
      })
    , [dispatch])
    let exportModel = React.useCallback1(id => {
      state
      ->State.model(id)
      ->Option.iter(model => {
        let model =
          model->State.Model.addToplevelNote(
            "\n=== Exported " ++ Js.Date.make()->Js.Date.toString ++ " ===",
          )
        let name = State.Model.info(model).name
        let json = State.Model.Stable.V5.toJson(model)
        let content =
          "data:text/json;charset=utf-8," ++ json->Js.Json.stringify->Js.Global.encodeURIComponent
        Downloader.download(name ++ ".risn", content)
      })
    }, [stateHash])

    Obj.magic(window)["downloadAllModels"] = () => {
      let zip = Zip.create()
      let stack = [zip->Zip.root]
      state
      ->State.models
      ->FileTree.asFlat
      ->Array.forEach(f =>
        switch f {
        | FileTree.FileOrFolder.File((_, model)) => {
            let name = model->State.Model.info->InspectorState.Model.name ++ ".risn"
            let contents = model->State.Model.Stable.V5.toJson->Js.Json.stringify
            stack[Array.length(stack) - 1]->Option.iter(folder =>
              folder->Zip.Folder.createFile(name, contents)
            )
          }
        | FileTree.FileOrFolder.Folder(name, _) =>
          stack[Array.length(stack) - 1]->Option.iter(folder => {
            let newf = folder->Zip.Folder.createFolder(name)
            stack->Js.Array2.push(newf)->ignore
          })
        | FileTree.FileOrFolder.EndFolder(_, _) => stack->Js.Array2.pop->ignore
        }
      )
      zip
      ->Zip.root
      ->Zip.Folder.createFile(
        "README",
        "RISN Editor all models, exported " ++ Js.Date.make()->Js.Date.toString,
      )
      zip
      ->Zip.generateAsync
      ->Promise.thenResolve(base64 => {
        let content = "data:application/zip;base64," ++ base64
        Downloader.download("RISE Models.zip", content)
      })
      ->ignore
    }

    React.useEffect2(() => {
      GlobalKeybindings.set([
        K.create(K.cmdOrCtrl() ++ "+z", undo),
        K.create(K.cmdOrCtrl() ++ "+Shift+z", redo),
        K.create(K.cmdOrCtrl() ++ "+y", redo),
      ])
      None
    }, (undo, redo))

    let adds = React.useMemo5(
      () => (addRepNodeAt, addSchNodeAt, addDimNodeAt, addTokNodeAt, addPlcNodeAt),
      (addRepNodeAt, addSchNodeAt, addDimNodeAt, addTokNodeAt, addPlcNodeAt),
    )
    let links = React.useMemo3(
      () => (connectNodes, anchorNodes, makeGenericLink),
      (connectNodes, anchorNodes, makeGenericLink),
    )
    let keybindings = React.useMemo6(
      () =>
        Js.Dict.fromArray([
          ("r", (_, ~x, ~y) => addRepNodeAt(~x, ~y)),
          ("s", (_, ~x, ~y) => addSchNodeAt(~x, ~y)),
          ("d", (_, ~x, ~y) => addDimNodeAt(~x, ~y)),
          ("t", (_, ~x, ~y) => addTokNodeAt(~x, ~y)),
          ("y", (_, ~x, ~y) => addTokNodeAt(~x, ~y)),
          ("q", (_, ~x, ~y) => addPlcNodeAt(~x, ~y)),
          ("c", (_, ~x as _, ~y as _) => connectNodes(false)),
          ("Shift+C", (_, ~x as _, ~y as _) => connectNodes(true)), // reversed direction
          ("a", (_, ~x as _, ~y as _) => anchorNodes(false)),
          ("Shift+A", (_, ~x as _, ~y as _) => anchorNodes(true)), // reversed direction
          ("g", (_, ~x as _, ~y as _) => makeGenericLink()),
          ("x", (_, ~x as _, ~y as _) => delete()),
          ("ArrowLeft", (_, ~x as _, ~y as _) => nudge(~dx=-10.0, ~dy=0.0)),
          ("ArrowRight", (_, ~x as _, ~y as _) => nudge(~dx=10.0, ~dy=0.0)),
          ("ArrowUp", (_, ~x as _, ~y as _) => nudge(~dx=0.0, ~dy=-10.0)),
          ("ArrowDown", (_, ~x as _, ~y as _) => nudge(~dx=0.0, ~dy=10.0)),
          ("Shift+ArrowLeft", (_, ~x as _, ~y as _) => nudge(~dx=-1.0, ~dy=0.0)),
          ("Shift+ArrowRight", (_, ~x as _, ~y as _) => nudge(~dx=1.0, ~dy=0.0)),
          ("Shift+ArrowUp", (_, ~x as _, ~y as _) => nudge(~dx=0.0, ~dy=-1.0)),
          ("Shift+ArrowDown", (_, ~x as _, ~y as _) => nudge(~dx=0.0, ~dy=1.0)),
          ("Backspace", (_, ~x as _, ~y as _) => delete()),
          ("Delete", (_, ~x as _, ~y as _) => delete()),
          ("v", (_, ~x as _, ~y as _) => unlinkNodes()),
          ("Ctrl+d", (_, ~x as _, ~y as _) => duplicateNodes()),
        ]),
      (adds, links, nudge, delete, unlinkNodes, duplicateNodes),
    )

    let (showGrid, setShowGrid) = React.useState(_ => {
      BoolStore.get("REP-SHOW-GRID")->Or_error.getWithDefault(false)
    })

    let toggleGrid = React.useCallback2(_ => {
      if showGrid {
        BoolStore.set("REP-SHOW-GRID", false)
        setShowGrid(_ => false)
      } else {
        BoolStore.set("REP-SHOW-GRID", true)
        setShowGrid(_ => true)
      }
    }, (showGrid, setShowGrid))

    let fpData = React.useMemo1(() => State.models(state), [stateHash])
    let graphData = React.useMemo2(
      () =>
        focused
        ->Option.flatMap(focused =>
          state
          ->State.model(focused)
          ->Option.map(model => model->State.Model.graph->ModelState.data)
        )
        ->Option.getWithDefault(ModelState.empty->ModelState.data),
      (focused, stateHash),
    )
    let modelName = React.useCallback0(model => model->State.Model.info->InspectorState.Model.name)
    let active = React.useMemo1(() => State.focused(state), [State.focused(state)])
    let importExtensions = React.useMemo0(() => [".risn", ".repn"])
    let graphStyle = React.useMemo0(() => ReactDOM.Style.make(~flexGrow="1", ()))

    let viewTransform = ref(None)
    ifChanged(() => {
      viewTransform :=
        focused
        ->Option.flatMap(state->State.viewTransform(_))
        ->Option.getWithDefault(ReactD3Graph.Graph.ViewTransform.init)
        ->Some
    }, focused)

    <main
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="row",
        ~fontFamily="sans-serif",
        ~height="100%",
        ~overflow="hidden",
        (),
      )}>
      <FP
        id="file-panel"
        title="RISN Editor"
        version="##VERSION##"
        data={fpData}
        dataName={modelName}
        active // Focused is always a model, so this is done separately.
        importExtensions
        onCreate={newModel}
        onCreateFolder={newFolder}
        onDelete={deleteModel}
        onDeleteFolder={deleteFolder}
        onSelect={focusModel}
        onDuplicate={duplicateModel}
        onChangedName={renameModel}
        onChangedFolderName={renameFolder}
        onReorder={reorder}
        onImport={importModels}
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
          ~overflow="hidden",
          (),
        )}>
        <div
          className="graph-header"
          style={ReactDOM.Style.make(
            ~order="1",
            ~display="flex",
            ~alignItems="center",
            ~minHeight="30px",
            ~borderBottom="1px solid black",
            ~padding="0 0.5rem",
            ~overflowX="auto",
            (),
          )}>
          <Button
            onClick={_ => undo()} value="Undo" enabled={canUndo} tooltip={K.cmdOrCtrl() ++ "+Z"}
          />
          <Button
            onClick={_ => redo()}
            value="Redo"
            enabled={canRedo}
            tooltip={K.cmdOrCtrl() ++ "+Shift+Z"}
          />
          <Button.Separator />
          <Button
            onClick={_ => addRepNodeAt(~x=0., ~y=0.)}
            value="Representation"
            enabled={toolbarActive}
            tooltip="R"
          />
          <Button
            onClick={_ => addSchNodeAt(~x=0., ~y=0.)}
            value="R-Scheme"
            enabled={toolbarActive}
            tooltip="S"
          />
          <Button
            onClick={_ => addDimNodeAt(~x=0., ~y=0.)}
            value="R-Dimension"
            enabled={toolbarActive}
            tooltip="D"
          />
          <Button
            onClick={_ => addTokNodeAt(~x=0., ~y=0.)}
            value="R-Symbol"
            enabled={toolbarActive}
            tooltip="Y"
          />
          <Button
            onClick={_ => addPlcNodeAt(~x=0., ~y=0.)}
            value="Placeholder"
            enabled={toolbarActive}
            tooltip="Q"
          />
          <Button.Separator />
          <Button
            onClick={_ => duplicateNodes()}
            value="Duplicate"
            enabled={toolbarActive}
            tooltip="Ctrl+D"
          />
          <Button.Separator />
          <Button
            onClick={_ => connectNodes(false)} value="Connect" enabled={toolbarActive} tooltip="C"
          />
          <Button
            onClick={_ => anchorNodes(false)} value="Anchor" enabled={toolbarActive} tooltip="A"
          />
          <Button
            onClick={_ => makeGenericLink()} value="Generic" enabled={toolbarActive} tooltip="G"
          />
          <Button.Separator />
          <Button onClick={_ => unlinkNodes()} value="Unlink" enabled={toolbarActive} tooltip="V" />
          <Button.Separator />
          <Button onClick={_ => delete()} value="Delete" enabled={toolbarActive} tooltip="X" />
          <Button.Separator />
          <label htmlFor="gridToggle"> {React.string("Grid")} </label>
          <input
            type_="checkbox"
            id="gridToggle"
            onChange={toggleGrid}
            checked={showGrid}
            style={ReactDOM.Style.make(
              ~marginLeft="0.5em",
              ~minWidth="12px",
              ~minHeight="12px",
              (),
            )}
          />
          <Button.Separator />
          <a href="manual.html" target="_blank"> {React.string("Manual")} </a>
          <Button.Separator />
          {if isRecording {
            <span
              onClick={_ => setIsRecording(false)}
              style={ReactDOM.Style.make(~padding="0 0.5rem", ())}>
              <svg width="10px" height="10px"> <rect width="10" height="10" fill="black" /> </svg>
            </span>
          } else {
            <span
              onClick={_ => setIsRecording(true)}
              style={ReactDOM.Style.make(~padding="0 0.5rem", ())}>
              <svg width="10px" height="10px"> <circle cx="5" cy="5" r="5" fill="#b00" /> </svg>
            </span>
          }}
        </div>
        <div
          className="container"
          style={ReactDOM.Style.make(
            ~order="2",
            ~flexGrow="1",
            ~overflow="hidden",
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
              ~overflow="hidden",
              (),
            )}>
            <Graph
              id={"model-graph"}
              config
              data={graphData}
              selection
              viewTransform=?{viewTransform.contents}
              onSelectionChange={selectionChange}
              onNodePositionChange={movedNodes}
              onZoomChange={onZoomed}
              keybindings={keybindings}
              showGrid
              style={graphStyle}
            />
            <IntelligenceUI
              intelligence={intel}
              lastRequestedIntelligence={focused->Option.flatMap(focused =>
                state->State.model(focused)->Option.flatMap(State.Model.requestedIntelligence)
              )}
              selected={focused->Option.flatMap(focused =>
                state->State.model(focused)->Option.flatMap(State.Model.focusedIntelligence)
              )}
              onClickError={clickError}
              onClickWarning={clickWarning}
              onClickInsight={clickInsight}
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
                let slots = model->State.Model.slotsForSelection(selection)
                switch slots {
                | [] => InspectorState.Global(State.Model.info(model))
                | [(id, InspectorState.SchemaOrLink.Schema(slot))] =>
                  InspectorState.Schema(id, slot)
                | [(id, InspectorState.SchemaOrLink.Link(slot))] => InspectorState.Link(id, slot)
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
| Some(e) => App.init->Promise.thenResolve(init => ReactDOM.render(<App init />, e))->ignore
}
