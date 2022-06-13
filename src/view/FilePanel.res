module FileLabel = {
  type state = {
    savedName: string,
    currName: string,
    editing: bool,
  }
  type action =
    | StartEdit
    | MidEdit(string)
    | EndEdit(string)

  let init = name => {savedName: name, currName: name, editing: false}
  let reducer = (state, action) =>
    switch action {
    | StartEdit => {...state, editing: true}
    | MidEdit(name) => {...state, currName: name}
    | EndEdit(newName) => init(newName)
    }

  @react.component
  let make = (~id, ~name, ~active, ~onSelect, ~onChanged, ~dragHandleProps) => {
    let (state, dispatch) = React.useReducer(reducer, init(name))
    let handleProps = {
      "style": ReactDOM.Style.make(
        ~color="rgba(100, 100, 100)",
        ~width="100%",
        ~height="100%",
        ~cursor="grab",
        (),
      ),
    }->Js.Obj.assign(dragHandleProps)
    let handle = React.cloneElement(
      // Vertical ellipsis
      <div> {React.string(Js.String2.fromCharCode(8942))} </div>,
      handleProps,
    )
    <span
      style={ReactDOM.Style.make(
        ~display="block",
        ~padding="0.5rem",
        ~overflow="hidden",
        ~textOverflow="ellipsis",
        ~whiteSpace="nowrap",
        ~background={
          if active {
            "lightgrey"
          } else {
            "white"
          }
        },
        (),
      )}
      id={"file-label-" ++ Gid.toString(id)}
      key={Gid.toString(id)}
      className={if active {
        "file-active"
      } else {
        "file-inactive"
      }}
      onClick={e => {
        ReactEvent.Mouse.stopPropagation(e)
        ReactEvent.Mouse.preventDefault(e)
        if !active {
          onSelect()
        }
      }}>
      <div
        style={ReactDOM.Style.make(
          ~width="6px",
          ~height="12px",
          ~display="inline-block",
          ~marginRight="0.5rem",
          ~marginTop="-0.5ex",
          (),
        )}>
        {handle}
      </div>
      {if state.editing {
        <input
          style={ReactDOM.Style.make(
            ~fontSize="1rem",
            ~padding="0",
            ~margin="0",
            ~borderWidth="0",
            ~width="calc(100% - 20px)",
            (),
          )}
          autoFocus={true}
          value={state.currName}
          onChange={e => dispatch(MidEdit(ReactEvent.Form.target(e)["value"]))}
          onKeyPress={e =>
            if ReactEvent.Keyboard.key(e) == "Enter" {
              let newName = ReactEvent.Keyboard.target(e)["value"]
              dispatch(EndEdit(newName))
              onChanged(newName)
            } else {
              ()
            }}
          onBlur={e => {
            let newName = ReactEvent.Focus.target(e)["value"]
            dispatch(EndEdit(newName))
            onChanged(newName)
          }}
        />
      } else {
        <span
          className={"inner-name-focus inner-name-not-editing"}
          title={state.currName}
          onDoubleClick={e => {
            ReactEvent.Mouse.preventDefault(e)
            ReactEvent.Mouse.stopPropagation(e)
            dispatch(StartEdit)
          }}>
          {React.string(state.currName)}
        </span>
      }}
    </span>
  }
}

module Template = {
  @react.component
  let make = (
    ~item as (id, model),
    ~itemSelected as _,
    ~anySelected as _,
    ~dragHandleProps,
    ~commonProps=?,
  ) => {
    let commonProps = Option.getExn(commonProps) // MUST have them, else we have nothing! Ahh!
    let active = commonProps["active"]
    let onSelect = commonProps["onSelect"]
    let onChangedName = commonProps["onChangedName"]
    let props = {
      "id": id,
      "name": {model->State.Model.info->InspectorState.Model.name},
      "active": {
        active->Option.map(active => id == active)->Option.getWithDefault(false)
      },
      "onSelect": {() => onSelect(Some(id))},
      "onChanged": {name => onChangedName(id, name)},
      "dragHandleProps": dragHandleProps,
    }
    React.createElement(FileLabel.make, props)
  }
}

@react.component
let make = (
  ~id,
  ~models: array<(Gid.t, State.Model.t)>,
  ~active,
  ~onCreate,
  ~onDelete,
  ~onSelect,
  ~onDuplicate,
  ~onChangedName,
  ~onReorder,
  ~onImport,
  ~onExport,
) => {
  let container = React.useRef(Js.Nullable.null)
  let (dropTargetActive, setDropTargetActive) = React.useState(() => false)
  <HideablePanel2
    id
    toggle={(~hidden) =>
      <div
        style={ReactDOM.Style.make(
          ~cursor="default",
          ~userSelect="none",
          ~position="absolute",
          ~top="40px",
          ~zIndex="100000",
          ~left={
            if hidden {
              "10px"
            } else {
              "230px"
            }
          },
          ~fontSize="16px",
          (),
        )}>
        {if hidden {
          React.string(Js.String2.fromCharCode(9002))
        } else {
          React.string(Js.String2.fromCharCode(9001))
        }}
      </div>}
    ref_={ReactDOM.Ref.domRef(container)}
    style={ReactDOM.Style.make(
      ~order="1",
      ~minWidth="230px",
      ~maxWidth="230px",
      ~display="flex",
      ~flexDirection="column",
      ~borderRight="1px solid black",
      (),
    )}>
    <h1 style={ReactDOM.Style.make(~padding="1rem", ())}> {React.string("RISN Editor")} </h1>
    <div
      style={ReactDOM.Style.make(
        ~fontSize="0.7rem",
        ~fontWeight="bold",
        ~textAlign="right",
        ~margin="-1.2rem 2.5rem 1rem 0",
        (),
      )}>
      {React.string("V ##VERSION##")}
    </div>
    <div
      style={ReactDOM.Style.make(
        ~flexGrow="1",
        ~display="flex",
        ~flexDirection="column",
        ~overflowY="auto",
        (),
      )}
      onClick={_ => onSelect(None)}>
      <ReactDraggableList.DraggableList
        items={models}
        itemKey={((id, _)) => id->Gid.toString}
        template={Template.make}
        onMoveEnd={(~newList, ~movedItem as _, ~oldIndex as _, ~newIndex as _) =>
          onReorder(newList->Array.map(((id, _)) => id))}
        container={() => container.current}
        constrainDrag={true}
        padding={0}
        commonProps={
          "active": active,
          "onSelect": onSelect,
          "onChangedName": onChangedName,
        }
      />
      <div
        style={ReactDOM.Style.make(
          ~flexGrow="1",
          ~minHeight="50px",
          ~background={
            if dropTargetActive {
              "rgba(0,0,0,0.1)"
            } else {
              "none"
            }
          },
          ~border={
            if dropTargetActive {
              "2px solid rgba(0,0,0,0.2)"
            } else {
              "none"
            }
          },
          (),
        )}
        onDragEnter={e => {
          ReactEvent.Mouse.preventDefault(e)
          setDropTargetActive(_ => true)
        }}
        onDragOver={e => ReactEvent.Mouse.preventDefault(e)}
        onDragLeave={e => setDropTargetActive(_ => false)}
        onDrop={e => {
          ReactEvent.Mouse.preventDefault(e)
          setDropTargetActive(_ => false)
          let files: array<File.t> = Obj.magic(e)["dataTransfer"]["files"] // Absolute hack
          let (keep, reject) = files->Array.partition(f => {
            let fname = File.name(f)
            fname->String.endsWith(".risn") || fname->String.endsWith(".repn")
          })
          if keep != [] {
            onImport(keep)
          }
          if reject != [] {
            Dialog.alert(
              "Could not upload files:\n" ++
              reject
              ->Array.map(f => "  " ++ File.name(f) ++ "\n")
              ->Js.Array2.joinWith("") ++ "Not '.risn' files.",
            )
          }
        }}
      />
    </div>
    <div
      className="file-controls"
      style={ReactDOM.Style.make(
        ~height="60px",
        ~borderTop="1px solid black",
        ~display="flex",
        ~flexDirection="column",
        ~alignItems="center",
        ~padding="0 0.5rem",
        (),
      )}>
      <div
        style={ReactDOM.Style.make(
          ~height="30px",
          ~display="flex",
          ~flexDirection="row",
          ~alignItems="center",
          ~width="100%",
          (),
        )}>
        <Button onClick={_ => onCreate()} value="New" />
        <Button onClick={_ => active->Option.iter(onDuplicate)} value="Duplicate" />
        <Button.Separator />
        <Button
          onClick={_ =>
            active->Option.iter(active => {
              let name =
                models
                ->Array.find(((id, _)) => id === active)
                ->Option.getExn
                ->(((_, model)) => model)
                ->State.Model.info
                ->InspectorState.Model.name
              if Dialog.confirm("Definitely delete model '" ++ name ++ "'?") {
                onDelete(active)
              }
            })}
          value="Delete"
        />
      </div>
      <div
        style={ReactDOM.Style.make(
          ~height="30px",
          ~display="flex",
          ~flexDirection="row",
          ~alignItems="center",
          ~width="100%",
          (),
        )}>
        <Button onClick={_ => active->Option.iter(onExport)} value="Export" />
        <input
          name="import_models"
          id="import_models"
          type_="file"
          accept=".risn,.repn"
          multiple={true}
          style={ReactDOM.Style.make(
            ~width="0.1px",
            ~height="0.1px",
            ~opacity="0",
            ~overflow="hidden",
            ~position="absolute",
            ~zIndex="-1",
            (),
          )}
          onChange={e => {
            let files = e->ReactEvent.Form.currentTarget->(t => t["files"])
            switch files {
            | [] => ()
            | fs => onImport(fs)
            }
          }}
        />
        <label htmlFor="import_models">
          <Button
            value="Import"
            onClick={e => {
              let label = ReactEvent.Mouse.target(e)["parentNode"]
              label["click"](.)
            }}
          />
        </label>
      </div>
    </div>
  </HideablePanel2>
}

let make = React.memoCustomCompareProps(make, (old_, new_) => {
  old_["id"] === new_["id"] &&
  old_["active"] === new_["active"] &&
  Array.length(old_["models"]) === Array.length(new_["models"]) &&
  Array.zip(old_["models"], new_["models"])->Array.every((((i, im), (j, jm))) =>
    i === j &&
      im->State.Model.info->InspectorState.Model.name ===
        jm->State.Model.info->InspectorState.Model.name
  )
})
