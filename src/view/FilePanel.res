let makeFileLabel = (id, name, isActive, onSelect) => {
  <span
    id={"file-label-" ++ Uuid.toString(id)}
    key={Uuid.toString(id)}
    className={if isActive {
      "file-active"
    } else {
      "file-inactive"
    }}
    onClick={_ => onSelect()}>
    {React.string(name)}
  </span>
}

@react.component
let make = (~id, ~models: array<State.Model.t>, ~active, ~onCreate, ~onDelete, ~onSelect) => {
  <div id>
    <div className="file-list">
      {models
      ->Array.map(model =>
        makeFileLabel(
          model->State.Model.id,
          model->State.Model.name,
          active
          ->Option.map(active => model->State.Model.id == active)
          ->Option.getWithDefault(false),
          () => onSelect(model->State.Model.id),
        )
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
