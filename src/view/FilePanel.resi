@react.component
let make: (
  ~id: string,
  ~models: array<State.Model.t>,
  ~active: option<Uuid.t>,
  ~onCreate: Uuid.t => unit,
  ~onDelete: Uuid.t => unit,
  ~onSelect: Uuid.t => unit,
  ~onChangedName: (Uuid.t, string) => unit,
  ~onReorder: array<State.Model.t> => unit,
) => React.element
