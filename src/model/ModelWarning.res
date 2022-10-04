include ModelError

let kindToString = kind =>
  switch kind {
  | #representation => "Representation"
  | #scheme => "R-Scheme"
  | #dimension => "R-Dimension"
  | #token => "R-Symbol"
  | #placeholder => "Placeholder"
  }

let defaultConceptWarning = (nodes, ~field, ~default, kind) => {
  let kind = kindToString(kind)
  create(
    ~nodes,
    ~message=kind ++ " is using default " ++ field ++ ".",
    ~details="We give each " ++
    kind ++
    " the default " ++
    field ++
    " \"" ++
    default ++ "\", but this is intended only as a placeholder.",
    ~suggestion="Replace this " ++ kind ++ " schema's " ++ field ++ ".",
    (),
  )
}

let defaultReferenceWarning = (nodes, label, kind) => {
  let kind = kindToString(kind)
  create(
    ~nodes,
    ~message=kind ++ " is using default " ++ label ++ ".",
    ~details="We give each " ++
    kind ++
    " the default " ++
    label ++ " \"#Ref#\", but this is intended only as a placeholder.",
    ~suggestion="Replace this " ++ kind ++ "schema's " ++ label ++ ".",
    (),
  )
}

let multipleRootsWarning = roots =>
  create(
    ~nodes=roots,
    ~message="More than one model detected.",
    ~details="There are multiple models in this file, or a model with multiple roots.",
    (),
  )
