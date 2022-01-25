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
          className={"inner-name-focus inner-name-editing"}
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
          className={"inner-name-focus inner-name-not-editing"} onClick={_ => dispatch(StartEdit)}>
          {React.string(state.currName)}
        </span>
      }}
    </span>
  }
}

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
  <div id>
    <div className="file-list">
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
    <div className="file-controls">
      <button onClick={_ => onCreate(Uuid.create())}> {React.string("New")} </button>
      <button onClick={_ => active->Option.iter(active => onDelete(active))}>
        {React.string("Delete")}
      </button>
    </div>
  </div>
}
