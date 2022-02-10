@react.component
let make: (
  ~id: string,
  ~models: array<State.Model.t>,
  ~active: option<Uuid.t>,
  ~onCreate: unit => unit,
  ~onDelete: Uuid.t => unit,
  ~onSelect: Uuid.t => unit,
  ~onDuplicate: Uuid.t => unit,
  ~onChangedName: (Uuid.t, string) => unit,
  ~onImport: File.t => unit,
  ~onExport: Uuid.t => unit,
) => React.element
