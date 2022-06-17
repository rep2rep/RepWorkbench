@react.component
let make: (
  ~id: string,
  ~models: FileTree.t<(Gid.t, State.Model.t)>,
  ~active: option<Gid.t>,
  ~onCreate: array<Gid.t> => unit,
  ~onCreateFolder: array<Gid.t> => unit,
  ~onDelete: Gid.t => unit,
  ~onDeleteFolder: Gid.t => unit,
  ~onSelect: option<Gid.t> => unit,
  ~onDuplicate: Gid.t => unit,
  ~onChangedName: (Gid.t, string) => unit,
  ~onChangedFolderName: (Gid.t, string) => unit,
  ~onReorder: FileTree.t<Gid.t> => unit,
  ~onImport: array<File.t> => unit,
  ~onExport: Gid.t => unit,
) => React.element
