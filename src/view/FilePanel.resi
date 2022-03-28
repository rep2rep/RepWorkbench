@react.component
let make: (
  ~id: string,
  ~models: array<(Gid.t, State.Model.t)>,
  ~active: option<Gid.t>,
  ~onCreate: unit => unit,
  ~onDelete: Gid.t => unit,
  ~onSelect: option<Gid.t> => unit,
  ~onDuplicate: Gid.t => unit,
  ~onChangedName: (Gid.t, string) => unit,
  ~onReorder: array<Gid.t> => unit,
  ~onImport: array<File.t> => unit,
  ~onExport: Gid.t => unit,
) => React.element
