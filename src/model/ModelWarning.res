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

let conceptFunctionWarning = (nodes, kind, func) => {
  let kind = kindToString(kind)
  let message = switch func {
  | Function.Arbitrary => kind ++ " Concept must be \"*NULL*\" because the Function is Arbitrary."
  | _ => kind ++ " Concept is \"*NULL*\", but the Function is not Arbitrary."
  }
  let details =
    kind ++ " schemas either have a concept, and are thus not arbitrary, or have no concept (signified by \"*NULL*\"), and so are functionally arbitrary."
  let suggestion = switch func {
  | Function.Arbitrary => "Set the Concept to \"*NULL\", or chance the Function."
  | _ => "Change the Concept from \"*NULL*\", or set the Function to Arbitrary."
  }
  create(~nodes, ~message, ~details, ~suggestion, ())
}

let graphicExplicitWarning = (nodes, kind, explicit) => {
  let kind = kindToString(kind)
  let message = if explicit {
    kind ++ " Graphic is \"*NULL*\", but it is marked as Explicit."
  } else {
    kind ++ " Graphic must be \"*NULL*\" because it is not marked as Explicit."
  }
  let details =
    kind ++ " schemas either have a graphic, and are thus explicit, or have no graphic (signified by \"*NULL*\"), and so are not explicit."
  let suggestion = if explicit {
    "Change the Graphic from \"*NULL*\", or set Explicit to No."
  } else {
    "Set the Graphic to \"*NULL\", or chance Explicit to Yes."
  }
  create(~nodes, ~message, ~details, ~suggestion, ())
}

let multipleRootsWarning = roots =>
  create(
    ~nodes=roots,
    ~message="More than one model detected.",
    ~details="There are multiple models in this file, or a model with multiple roots.",
    (),
  )
