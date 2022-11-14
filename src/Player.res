module Player = {
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

  exception RecordingError(Error.t)
  let reducer = ((states, i), next_i) =>
    if next_i >= Array.length(states) || next_i < 0 {
      (states, i)
    } else {
      (states, next_i)
    }

  let config = ReactD3Graph.Config.create(
    ~global=ReactD3Graph.Config.Global.create(
      ~panAndZoom=false,
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

  module Playbar = {
    let msToMMSS = time => {
      let time = Float.toInt(time /. 1000.)
      let seconds = mod(time, 60)
      let minutes = time / 60
      let minutes_string = Int.toString(minutes)
      let seconds_string = {
        let s = Int.toString(seconds)
        if String.length(s) < 2 {
          "0" ++ s
        } else {
          s
        }
      }
      minutes_string ++ ":" ++ seconds_string
    }

    @react.component
    let make = (~playing, ~timestamp, ~onPlay, ~onPause, ~onScrub, ~maxTime) => {
      <>
        {if playing {
          <Button onClick=onPause value="Pause" style={ReactDOM.Style.make(~width="4em", ())} />
        } else {
          <Button onClick=onPlay value="Play" style={ReactDOM.Style.make(~width="4em", ())} />
        }}
        <input
          type_="range"
          min="0"
          max={Float.toString(maxTime)}
          value={Float.toString(timestamp)}
          step=0.001
          onChange=onScrub
          style={ReactDOM.Style.make(~width="300px", ~margin="0px 1em", ())}
        />
        {React.string(msToMMSS(timestamp) ++ "/" ++ msToMMSS(maxTime))}
      </>
    }
  }

  let playInterval = ref(None)

  let findStateIndexForTime = (states, timestamp) => {
    let left = ref(0)
    let right = ref(Array.length(states))
    while right.contents - left.contents > 1 {
      let mid = (left.contents + right.contents) / 2
      let (ts, _) = states->Array.get(mid)->Option.getExn
      if ts === timestamp {
        left := mid
        right := mid
      } else if ts < timestamp {
        left := mid
      } else {
        right := mid
      }
    }
    left.contents
  }

  @react.component
  let make = (~recording, ~filename, ~onChangeRecording) => {
    let init = React.useMemo1(() => {
      Recording.unwind(recording)
    }, [recording])
    let ((states, index), dispatch) = React.useReducer(reducer, (init, 0))
    let (stateTimestamp, state) = states->Array.get(index)->Option.getExn

    let (timestamp, setTimestamp) = React.useState(_ => stateTimestamp)
    let (playing, setPlaying) = React.useState(_ => false)
    let (playspeed, setPlayspeed) = React.useState(_ => 1.)
    let maxTime = states->Array.get(Array.length(states) - 1)->Option.getExn->fst

    React.useEffect1(() => {
      if (
        stateTimestamp > timestamp ||
          states->Array.get(index + 1)->Option.map(fst)->Option.getWithDefault(0.) < timestamp
      ) {
        setTimestamp(_ => stateTimestamp)
      }
      None
    }, [stateTimestamp])
    React.useEffect1(() => {
      let newIndex = findStateIndexForTime(states, timestamp)
      if newIndex != index {
        dispatch(newIndex)
      }
      None
    }, [timestamp])

    React.useEffect0(() => {
      Some(() => playInterval.contents->Option.iter(Js.Global.clearInterval))
    })

    let onPause = _ => {
      setPlaying(_ => false)
      playInterval.contents->Option.iter(Js.Global.clearInterval)
      playInterval := None
    }
    let onPlay = _ => {
      if timestamp >= maxTime {
        setTimestamp(_ => 0.)
      }
      setPlaying(_ => true)
      playInterval := Some(Js.Global.setInterval(() => {
            setTimestamp(t => {
              if t >= maxTime {
                onPause()
              }
              Js.Math.min_float(t +. 50. *. playspeed, maxTime)
            })
          }, 50))
    }
    let onScrub = e => {
      setTimestamp(_ => ReactEvent.Form.target(e)["value"]->Float.fromString->Option.getExn)
    }

    let stateHash = State.hash(state)

    let focused = React.useMemo1(
      () =>
        state
        ->State.focused
        ->Option.flatMap(id => state->State.model(id))
        ->Option.flatMap(_ => state->State.focused),
      [state->State.focused],
    )
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
    let graphStyle = React.useMemo0(() => ReactDOM.Style.make(~flexGrow="1", ()))

    let downloadAllModels = () => {
      let zip = Zip.create()
      let stack = [zip->Zip.root]
      state
      ->State.models
      ->FileTree.asFlat
      ->Array.forEach(f =>
        switch f {
        | FileTree.FileOrFolder.File((_, model)) => {
            let name = model->State.Model.info->InspectorState.Model.name ++ ".risn"
            let contents = model->State.Model.Stable.V6.toJson->Js.Json.stringify
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
        Downloader.download(
          filename ++
          " " ++
          Int.toString(index + 1) ++
          "_of_" ++
          Int.toString(Array.length(states) - 1) ++ ".zip",
          content,
        )
      })
      ->ignore
    }

    let viewTransform =
      focused
      ->Option.flatMap(state->State.viewTransform(_))
      ->Option.getWithDefault(ViewTransform.init)

    let ignore2 = (_, _) => ()

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
        importExtensions=[]
        onCreate=ignore
        onCreateFolder=ignore
        onDelete=ignore
        onDeleteFolder=ignore
        onSelect=ignore
        onDuplicate=ignore
        onChangedName=ignore2
        onChangedFolderName=ignore2
        onReorder=ignore
        onImport=ignore2
        onExport=ignore
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
          <Playbar onPlay onPause onScrub timestamp playing maxTime />
          <input
            type_="number"
            min="0.1"
            max="100"
            step={0.1}
            value={Float.toString(playspeed)}
            onChange={e => setPlayspeed(ReactEvent.Form.target(e)["value"])}
            style={ReactDOM.Style.make(~marginLeft="1em", ())}
          />
          <Button.Separator />
          <Button onClick={_ => dispatch(0)} value={"<<"} />
          <Button onClick={_ => dispatch(index - 1)} value={"<"} />
          {React.string(Int.toString(index + 1) ++ "/" ++ Int.toString(Array.length(states)))}
          <Button onClick={_ => dispatch(index + 1)} value={">"} />
          <Button onClick={_ => dispatch(Array.length(states) - 1)} value={">>"} />
          <Button.Separator />
          <Button onClick={_ => downloadAllModels()} value="Download all models at current time" />
          <Button.Separator />
          <Button onClick={onChangeRecording} value="Open Recording" />
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
              viewTransform
              onSelectionChange={(~oldSelection as _, ~newSelection as _) => ()}
              onNodePositionChange={(_, ~x as _, ~y as _) => ()}
              onZoomChange={(~oldZoom as _, ~newZoom as _) => ()}
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
            />
          </div>
          <InspectorPanel
            id={"node_inspector"}
            onChange=ignore
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

module Uploader = {
  @react.component
  let make = (~onUpload, ~error=?) => {
    <div
      style={ReactDOM.Style.make(
        ~textAlign="center",
        ~fontFamily="sans-serif",
        ~margin="2rem auto ",
        ~padding="1rem",
        (),
      )}>
      <div style={ReactDOM.Style.make(~margin="2rem auto", ())}>
        <h1 style={ReactDOM.Style.make(~margin="3rem 0 0.5rem 0", ())}>
          {React.string("RISE Playback Viewer")}
        </h1>
        <div style={ReactDOM.Style.make(~fontSize="0.7rem", ~fontWeight="bold", ())}>
          {React.string("##VERSION##")}
        </div>
      </div>
      {switch error {
      | None => React.null
      | Some(msg) =>
        <div
          style={ReactDOM.Style.make(
            ~maxWidth="450px",
            ~margin="1rem auto",
            ~padding="1rem",
            ~border="1px solid #a00",
            ~borderRadius="5px",
            ~background="#fdd",
            ~color="#a00",
            (),
          )}>
          {React.string(msg)}
        </div>
      }}
      <p style={ReactDOM.Style.make(~padding="1rem", ())}>
        {React.string("Please upload a .risnrec file recorded using the usual RISN Editor.")}
      </p>
      <input
        type_="file"
        accept=".risnrec"
        multiple={false}
        onChange={e => {
          let files = e->ReactEvent.Form.currentTarget->(t => t["files"])
          switch files {
          | [f] => onUpload(f)
          | _ => ()
          }
        }}
      />
    </div>
  }
}

module Loader = {
  type state =
    | NoRecording
    | RecordingError(string)
    | Recording(string, Recording.t)

  @react.component
  let make = () => {
    let (recording, setRecording) = React.useState(_ => NoRecording)
    let onUpload = file => {
      file
      ->File.arrayBuffer
      ->Promise.then(Zip.loadAsync)
      ->Promise.thenResolve(zip =>
        zip
        ->Zip.root
        ->Zip.Folder.get("0")
        ->Option.iter(file =>
          file
          ->Zip.File.text
          ->Promise.thenResolve(text => {
            switch text->Js.Json.parseExn->Recording.fromJson->Or_error.match {
            | Or_error.Ok(r) => Recording(file->Zip.File.name, r)
            | Or_error.Err(e) => e->Error.toString->RecordingError
            }
            ->((x, _) => x)
            ->setRecording
          })
          ->ignore
        )
        ->ignore
      )
      ->ignore
    }
    switch recording {
    | NoRecording => <Uploader onUpload />
    | RecordingError(error) => <Uploader onUpload error />
    | Recording(filename, recording) =>
      <Player recording filename onChangeRecording={_ => setRecording(_ => NoRecording)} />
    }
  }
}

switch ReactDOM.querySelector("#root") {
| None => ()
| Some(e) => ReactDOM.render(<Loader />, e)
}
