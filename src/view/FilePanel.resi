@react.component
let make: (
  ~id: string,
  ~models: array<(Uuid.t, State.Model.t)>,
  ~active: option<Uuid.t>,
  ~onCreate: unit => unit,
  ~onDelete: Uuid.t => unit,
  ~onSelect: option<Uuid.t> => unit,
  ~onDuplicate: Uuid.t => unit,
  ~onChangedName: (Uuid.t, string) => unit,
  ~onReorder: array<Uuid.t> => unit,
  ~onImport: File.t => unit,
  ~onExport: Uuid.t => unit,
) => React.element
