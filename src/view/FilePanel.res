module GIDArrayStore = LocalStorage.MakeJsonable({
  type t = array<Gid.t>

  let toJson = t => t->Array.toJson(Gid.toJson)
  let fromJson = json => json->Array.fromJson(Gid.fromJson)
})

module EditableLabel = {
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
  let make = (~value, ~onChanged, ~adjust) => {
    let (state, dispatch) = React.useReducer(reducer, init(value))
    if state.editing {
      <input
        style={ReactDOM.Style.make(
          ~fontSize="1rem",
          ~padding="0",
          ~margin="0",
          ~borderWidth="0",
          ~width="calc(100%" ++ adjust ++ ")",
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
    }
  }
}

module DragHandle = {
  @react.component
  let make = (~dragHandleProps) => {
    let handleProps = {
      "style": ReactDOM.Style.make(
        ~color="rgba(100, 100, 100)",
        ~width="100%",
        ~height="100%",
        ~cursor="grab",
        (),
      ),
    }->Js.Obj.assign(dragHandleProps)
    React.cloneElement(
      // Vertical ellipsis
      <div> {React.string(Js.String2.fromCharCode(8942))} </div>,
      handleProps,
    )
  }
}

module FileLabel = {
  @react.component
  let make = (~id, ~name, ~indent, ~active, ~onSelect, ~onChanged, ~dragHandleProps) => {
    <span
      style={ReactDOM.Style.make(
        ~display="block",
        ~padding="0.5rem",
        ~paddingLeft={Float.toString(0.75 *. Int.toFloat(indent) +. 0.5) ++ "rem"},
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
        <DragHandle dragHandleProps />
      </div>
      <EditableLabel value={name} onChanged adjust=" - 20px" />
    </span>
  }
}

module FolderLabel = {
  module DisclosureButton = {
    @react.component
    let make = (~isClosed, ~onToggleShow) => {
      let style = ReactDOM.Style.make(
        ~fontSize="0.7rem",
        ~opacity="0.7",
        ~cursor="default",
        ~display="inline-block",
        ~width="1.5em",
        ~overflow="hidden",
        ~position="relative",
        ~top="0.05rem",
        (),
      )
      let style = if isClosed {
        style
      } else {
        ReactDOM.Style.combine(style, ReactDOM.Style.make(~top="0.15rem", ()))
      }
      <span
        style
        onClick={e => {
          ReactEvent.Mouse.stopPropagation(e)
          onToggleShow()
        }}>
        {if isClosed {
          // Right triangle
          React.string(Js.String2.fromCharCode(9654))
        } else {
          // Down Triangle
          React.string(Js.String2.fromCharCode(9660))
        }}
      </span>
    }
  }

  @react.component
  let make = (
    ~id,
    ~name,
    ~indent,
    ~active,
    ~isClosed,
    ~onSelect,
    ~onChanged,
    ~dragHandleProps,
    ~onToggleShow,
  ) => {
    <span
      style={ReactDOM.Style.make(
        ~display="block",
        ~padding="0.5rem",
        ~paddingLeft={Float.toString(0.75 *. Int.toFloat(indent) +. 0.5) ++ "rem"},
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
        <DragHandle dragHandleProps />
      </div>
      <DisclosureButton onToggleShow isClosed />
      <EditableLabel value={name} onChanged adjust=" - 40px" />
    </span>
  }
}

module FolderEnd = {
  @react.component
  let make = (~id, ~indent, ~currentlyDragging) => {
    <div
      style={if currentlyDragging > 0.00000001 {
        ReactDOM.Style.make(
          ~height="2px",
          ~background="#ddd",
          ~marginLeft={Float.toString(0.75 *. Int.toFloat(indent) +. 0.5) ++ "rem"},
          ~opacity={Float.toString(currentlyDragging)},
          (),
        )
      } else {
        ReactDOM.Style.make(~height="2px", ())
      }}
      key={Gid.toString(id) ++ "_end"}
      id={"file-label-" ++ Gid.toString(id) ++ "_end"}
    />
  }
}

module Template = {
  @react.component
  let make = (
    ~item as (path, fileOrFolder),
    ~itemSelected as _,
    ~anySelected,
    ~dragHandleProps,
    ~commonProps=?,
  ) => {
    let commonProps = Option.getExn(commonProps) // MUST have them, else we have nothing! Ahh!
    let active = commonProps["active"]
    let onSelect = commonProps["onSelect"]
    let onChangedName = commonProps["onChangedName"]
    let onChangedFolderName = commonProps["onChangedFolderName"]
    let onToggleShow = commonProps["onToggleShow"]
    let closedFolders = commonProps["closedFolders"]
    if path->Array.some(v => closedFolders->Array.includes(v)) {
      // Hide this, but need some "dummies" to keep the indexing straight.
      let key = switch fileOrFolder {
      | FileTree.FileOrFolder.File((id, _)) => id->Gid.toString
      | FileTree.FileOrFolder.Folder(_, id) => id->Gid.toString
      | FileTree.FileOrFolder.EndFolder(_, id) => id->Gid.toString ++ "_end"
      }
      <span key id={"file-label-" ++ key} />
    } else {
      switch fileOrFolder {
      | FileTree.FileOrFolder.File((id, model)) => {
          let name = model->State.Model.info->InspectorState.Model.name
          let props = {
            "id": id,
            "indent": Array.length(path),
            "name": name,
            "active": {
              active->Option.map(active => id == active)->Option.getWithDefault(false)
            },
            "onSelect": {() => onSelect(Some(id))},
            "onChanged": {name => onChangedName(id, name)},
            "dragHandleProps": dragHandleProps,
          }
          React.createElement(FileLabel.make, props)
        }
      | FileTree.FileOrFolder.Folder(name, id) => {
          let props = {
            "id": id,
            "indent": Array.length(path),
            "name": name,
            "active": {
              active->Option.map(active => id == active)->Option.getWithDefault(false)
            },
            "onSelect": {() => onSelect(Some(id))},
            "onChanged": {name => onChangedFolderName(id, name)},
            "dragHandleProps": dragHandleProps,
            "onToggleShow": {() => onToggleShow(id)},
            "isClosed": closedFolders->Array.includes(id),
          }
          React.createElement(FolderLabel.make, props)
        }
      | FileTree.FileOrFolder.EndFolder(_, id) => {
          let props = {
            "id": id,
            "indent": Array.length(path) + 1,
            "currentlyDragging": anySelected,
          }
          React.createElement(FolderEnd.make, props)
        }
      }
    }
  }
}

let findEndFolder = (paths, id) =>
  paths->Array.getIndexBy(f =>
    switch f {
    | FileTree.FileOrFolder.EndFolder(id', _) => id === id'
    | _ => false
    }
  )

let move = (paths, ~oldIndex, ~newIndex) => {
  let movedItem = paths[oldIndex]->Option.getExn
  switch movedItem {
  | FileTree.FileOrFolder.File(_) => {
      let before = paths->Array.slice(~offset=0, ~len=oldIndex)
      let after = paths->Array.sliceToEnd(oldIndex + 1)
      let paths' = Array.concat(before, after)
      let before' = paths'->Array.slice(~offset=0, ~len=newIndex)
      let after' = paths'->Array.sliceToEnd(newIndex)
      Array.concatMany([before', [movedItem], after'])
    }
  | FileTree.FileOrFolder.Folder(id, _) =>
    findEndFolder(paths, id)
    ->Option.map(endIndex =>
      if newIndex > oldIndex && endIndex >= newIndex {
        paths
      } else if newIndex === oldIndex {
        paths
      } else {
        let movedChunk = paths->Array.slice(~offset=oldIndex, ~len=endIndex - oldIndex + 1)
        let before = paths->Array.slice(~offset=0, ~len=oldIndex)
        let after = paths->Array.sliceToEnd(endIndex + 1)
        let paths' = Array.concat(before, after)
        // The 'newIndex' assumed one thing moved, but actually many things have moved.
        // If we're moving earlier, no worries, all works. But if we're moving later, big problems.
        // To account for this, we assume that the newIndex is wrong by however long the movedChunk is, less 1.
        let newIndex = if newIndex < oldIndex {
          newIndex
        } else {
          newIndex - Array.length(movedChunk) + 1
        }
        let before' = paths'->Array.slice(~offset=0, ~len=newIndex)
        let after' = paths'->Array.sliceToEnd(newIndex)
        Array.concatMany([before', movedChunk, after'])
      }
    )
    ->Option.getWithDefault(paths)
  | _ => paths // Really shouldn't happen!
  }
}

@react.component
let make = (
  ~id,
  ~models: FileTree.t<(Gid.t, State.Model.t)>,
  ~active,
  ~onCreate,
  ~onCreateFolder,
  ~onDelete,
  ~onDeleteFolder,
  ~onSelect,
  ~onDuplicate,
  ~onChangedName,
  ~onChangedFolderName,
  ~onReorder,
  ~onImport,
  ~onExport,
) => {
  let isModel =
    active
    ->Option.map(active =>
      models->FileTree.getFolderPathAndPosition(active)->Option.isSome->Bool.not
    )
    ->Option.getWithDefault(false)
  let selectedPath =
    active
    ->Option.flatMap(id =>
      if isModel {
        models->FileTree.getFilePathAndPosition(((id', _)) => id === id')->Option.map(fst)
      } else {
        models
        ->FileTree.getFolderPathAndPosition(id)
        ->Option.map(fst)
        ->Option.map(path => path->FileTree.Path.extend(id))
      }
    )
    ->Option.getWithDefault(FileTree.Path.root)
  let container = React.useRef(Js.Nullable.null)
  let (dropTargetActive, setDropTargetActive) = React.useState(() => false)
  let (closedFolders, setClosedFolders') = React.useState(() =>
    GIDArrayStore.get("REPN_CLOSED_FOLDERS")->Or_error.getWithDefault([])
  )
  let setClosedFolders = v => {
    GIDArrayStore.set("REPN_CLOSED_FOLDERS", v)
    setClosedFolders'(_ => v)
  }
  let onToggleShow = id =>
    if closedFolders->Array.includes(id) {
      closedFolders->Array.filter(v => v !== id)->setClosedFolders
    } else {
      closedFolders->Array.concat([id])->setClosedFolders
    }
  let path = []
  let paths =
    models
    ->FileTree.asFlat
    ->Array.map(f =>
      switch f {
      | FileTree.FileOrFolder.File((_, _)) => (Array.copy(path), f)
      | FileTree.FileOrFolder.Folder(_, id) => {
          let d = Array.copy(path)
          path->Js.Array2.push(id)->ignore
          (d, f)
        }
      | FileTree.FileOrFolder.EndFolder(_, _) => {
          path->Js.Array2.pop->ignore
          let d = Array.copy(path)
          (d, f)
        }
      }
    )
  let commonProps = {
    "active": active,
    "closedFolders": closedFolders,
    "onSelect": onSelect,
    "onChangedName": onChangedName,
    "onChangedFolderName": onChangedFolderName,
    "onToggleShow": onToggleShow,
  }
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
        items={paths}
        itemKey={((_, f)) =>
          switch f {
          | FileTree.FileOrFolder.File((id, _)) => id->Gid.toString
          | FileTree.FileOrFolder.Folder(_, id) => id->Gid.toString
          | FileTree.FileOrFolder.EndFolder(_, id) => id->Gid.toString ++ "_end"
          }}
        template={Template.make}
        onMoveEnd={(~newList as _, ~movedItem as _, ~oldIndex, ~newIndex) =>
          paths
          ->Array.map(snd)
          ->move(~oldIndex, ~newIndex)
          ->FileTree.fromFlat
          ->FileTree.map(((id, _)) => id)
          ->onReorder}
        container={() => container.current}
        constrainDrag={true}
        padding={0}
        commonProps
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
            onImport(keep, FileTree.Path.root)
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
        <Button onClick={_ => onCreate(selectedPath)} value="New" />
        <Button
          onClick={_ => active->Option.iter(onDuplicate)} enabled={isModel} value="Duplicate"
        />
        <Button onClick={_ => onCreateFolder(selectedPath)} value="Folder" />
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
        <Button onClick={_ => active->Option.iter(onExport)} enabled={isModel} value="Export" />
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
            | fs => onImport(fs, selectedPath)
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
        <Button.Separator />
        <Button
          onClick={_ =>
            active->Option.iter(active => {
              paths->Array.forEach(((_, f)) => {
                switch f {
                | FileTree.FileOrFolder.File((id, model)) =>
                  if id === active {
                    let name = model->State.Model.info->InspectorState.Model.name
                    if Dialog.confirm("Definitely delete model '" ++ name ++ "'?") {
                      onDelete(id)
                    }
                  }
                | FileTree.FileOrFolder.Folder(name, id) =>
                  if id === active {
                    let contents = models->FileTree.folderContents(id)
                    if contents->Option.map(FileTree.isEmpty)->Option.getWithDefault(false) {
                      onDeleteFolder(id)
                      closedFolders->Array.filter(v => v !== id)->setClosedFolders
                    } else if (
                      Dialog.confirm(
                        "Delete the folder '" ++ name ++ "', along with all the models inside it?",
                      )
                    ) {
                      contents->Option.iter(c =>
                        c->FileTree.flatten->Array.forEach(((id, _)) => onDelete(id))
                      )
                      onDeleteFolder(id)
                      closedFolders->Array.filter(v => v !== id)->setClosedFolders
                    }
                  }
                | FileTree.FileOrFolder.EndFolder(_, _) => ()
                }
              })
            })}
          enabled={Option.isSome(active)}
          value="Delete"
        />
      </div>
    </div>
  </HideablePanel2>
}

let make = React.memoCustomCompareProps(make, (old_, new_) => {
  let old_models = FileTree.asFlat(old_["models"])
  let new_models = FileTree.asFlat(new_["models"])
  old_["id"] === new_["id"] &&
  old_["active"] === new_["active"] &&
  Array.length(old_models) === Array.length(new_models) &&
  Array.zip(old_models, new_models)->Array.every(((i, j)) =>
    switch (i, j) {
    | (FileTree.FileOrFolder.File((i, im)), FileTree.FileOrFolder.File((j, jm))) =>
      i === j &&
        im->State.Model.info->InspectorState.Model.name ===
          jm->State.Model.info->InspectorState.Model.name
    | (FileTree.FileOrFolder.Folder(i, id), FileTree.FileOrFolder.Folder(j, jd))
    | (FileTree.FileOrFolder.EndFolder(i, id), FileTree.FileOrFolder.EndFolder(j, jd)) =>
      i === j && id === jd
    | _ => false
    }
  )
})
