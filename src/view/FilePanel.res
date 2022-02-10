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
  let make = (~id, ~name, ~active, ~onSelect, ~onChanged) => {
    let (state, dispatch) = React.useReducer(reducer, init(name))
    <span
      style={ReactDOM.Style.make(
        ~display="block",
        ~padding="0.5rem",
        ~background={
          if active {
            "lightgrey"
          } else {
            "white"
          }
        },
        (),
      )}
      id={"file-label-" ++ Uuid.toString(id)}
      key={Uuid.toString(id)}
      className={if active {
        "file-active"
      } else {
        "file-inactive"
      }}
      onClick={_ => onSelect()}>
      {if state.editing {
        <input
          style={ReactDOM.Style.make(
            ~fontSize="1rem",
            ~padding="0",
            ~margin="0",
            ~borderWidth="0",
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
          onDoubleClick={_ => dispatch(StartEdit)}>
          {React.string(state.currName)}
        </span>
      }}
    </span>
  }
}

@val external confirm: string => bool = "confirm"

@react.component
let make = (
  ~id,
  ~models: array<State.Model.t>,
  ~active,
  ~onCreate,
  ~onDelete,
  ~onSelect,
  ~onDuplicate,
  ~onChangedName,
  ~onImport,
  ~onExport,
) => {
  <div
    id
    style={ReactDOM.Style.make(
      ~order="1",
      ~width="230px",
      ~display="flex",
      ~flexDirection="column",
      ~borderRight="1px solid black",
      (),
    )}>
    <h1 style={ReactDOM.Style.make(~padding="1rem", ())}> {React.string("RepNotation")} </h1>
    <div
      className="file-list"
      style={ReactDOM.Style.make(~flexGrow="1", ~display="flex", ~flexDirection="column", ())}>
      {models
      ->Array.map(model =>
        <FileLabel
          id={model->State.Model.id}
          name={model->State.Model.name}
          active={active
          ->Option.map(active => model->State.Model.id == active)
          ->Option.getWithDefault(false)}
          onSelect={() => onSelect(model->State.Model.id)}
          onChanged={name => onChangedName(model->State.Model.id, name)}
        />
      )
      ->React.array}
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
                ->Array.find(m => State.Model.id(m) == active)
                ->Option.getExn
                ->State.Model.name
              if confirm("Definitely delete model '" ++ name ++ "'?") {
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
          accept=".repn"
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
            | [f] => onImport(f)
            | _ => ()
            }
          }}
        />
        <label
          htmlFor="import_models"
          style={ReactDOM.Style.make(
            ~appearance="push-button",
            ~fontSize="small",
            ~cursor="default",
            (),
          )->ReactDOM.Style.unsafeAddStyle({
            "WebkitAppearance": "push-button",
            "MozAppearance": "push-button",
            "MsAppearance": "push-button",
            "OAppearance": "push-button",
          })}>
          {React.string("Import")}
        </label>
      </div>
    </div>
  </div>
}
