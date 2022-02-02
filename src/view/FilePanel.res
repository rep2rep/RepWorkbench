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
  ~onChangedName,
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
        ~height="50px",
        ~borderTop="1px solid black",
        ~display="flex",
        ~alignItems="center",
        ~padding="0 0.5rem",
        (),
      )}>
      <Button onClick={_ => onCreate(Uuid.create())}> {React.string("New")} </Button>
      <Button.Separator />
      <Button
        onClick={_ =>
          active->Option.iter(active => {
            let name =
              models->Array.find(m => State.Model.id(m) == active)->Option.getExn->State.Model.name
            if confirm("Definitely delete model '" ++ name ++ "'?") {
              onDelete(active)
            }
          })}>
        {React.string("Delete")}
      </Button>
    </div>
  </div>
}
