module Conv = {
  let internalError = (code, details) =>
    ModelError.create(
      ~nodes=[],
      ~message="Internal Error: " ++ code,
      ~details,
      ~suggestion="Notify Aaron about this so he can fix it :-) ",
      (),
    )

  let needsParentError = nodes =>
    ModelError.create(
      ~nodes,
      ~message="Schema has no parent, but it needs one.",
      ~details="All schemas, except for Representation schemas, must either be in the hierarchy, or be anchored below a Token in the hierarchy. This schema is neither in the hierarchy, nor anchored.",
      ~suggestion="Connect this schema below another schema.",
      (),
    )

  let noFunctionError = nodes =>
    ModelError.create(
      ~nodes,
      ~message="Missing \"Function\" of schema.",
      ~details="This schema requires a \"Function\" – whether it is semantic, auxiliary, or arbitrary. This has not been set.",
      ~suggestion="Select the appropriate \"Function\" from the dropdown menu.",
      (),
    )

  let noExplicitError = nodes =>
    ModelError.create(
      ~nodes,
      ~message="Missing whether schema is \"Explicit\".",
      ~details="This schema needs to be marked as \"Explicit\", or not, depending on whether it is explicit in the representation. This has not been set.",
      ~suggestion="Select the appropriate \"Explicit\" value (Yes or No) from the dropdown menu.",
      (),
    )

  let noScopeError = nodes =>
    ModelError.create(
      ~nodes,
      ~message="Missing the \"Scope\" of the schema.",
      ~details="This schema is either \"Global\" or \"Local\" in \"Scope\". This has not been set.",
      ~suggestion="Select the appropriate \"Scope\" value (Global or Local) from the dropdown menu.",
      (),
    )

  let noQuantityScaleError = (nodes, kind) =>
    ModelError.create(
      ~nodes,
      ~message="Missing the \"" ++ kind ++ " Scale\" of the R-dimension.",
      ~details="This R-dimension's \"" ++
      kind ++ " Scale\" must be one of \"Nominal\", \"Ordinal\", \"Interval\", or \"Ratio\". This has not been set.",
      ~suggestion="Select the appropriate \"" ++ kind ++ " Scale\" value from the dropdown menu.",
      (),
    )

  let defaultReferenceWarning = (nodes, label, kind) =>
    ModelWarning.create(
      ~nodes,
      ~message=kind ++ " is using default " ++ label ++ ".",
      ~details="We give each " ++
      kind ++
      " the default " ++
      label ++ " \"#Ref#\", but this is intended only as a placeholder.",
      ~suggestion="Replace this " ++ kind ++ "schema's " ++ label ++ ".",
      (),
    )

  let combineMessages = msgs =>
    msgs->Array.reduce(([], []), ((errs, warns), (e, w)) => (
      Array.concat(errs, e),
      Array.concat(warns, w),
    ))

  let toposort = (links, all_ids) => {
    let result = []
    let links = links->Array.filter(((_, _, k)) => k !== ModelLink.Kind.Relation)
    let roots =
      all_ids->Array.filter(id => links->Array.find(((_, id', _)) => id === id')->Option.isNone)
    let firstRoots = roots->Array.copy
    let rec f = links => {
      if Array.length(roots) !== 0 {
        let root = Js.Array2.pop(roots)->Option.getExn
        result->Js.Array2.push(root)->ignore
        links
        ->Array.filter(l => {
          let (s, t, _) = l
          if s === root {
            switch links->Array.filter(((_, x, _)) => t === x) {
            | [] => roots->Js.Array2.push(t)->ignore
            | [l'] =>
              if l' === l {
                roots->Js.Array2.push(t)->ignore
              }
            | _ => ()
            }
            false
          } else {
            true
          }
        })
        ->f
      }
    }
    f(links)
    if Array.length(links) === 0 {
      let errs =
        links->Array.map(((s, t, _)) =>
          ModelError.create(
            ~nodes=[s, t],
            ~message="Model has a cycle.",
            ~details="Models should not contain cycles – that is, the hierarchy and anchoring links should always connect a higher schema to a lower schema. We have found a situation where a lower schema is connected to a higher schema.",
            ~suggestion="We've detected a cycle involving these nodes, but it might not be here precisely. Find the link going the 'wrong way', and remove it.",
            (),
          )
        )
      (firstRoots, result, errs)
    } else {
      (firstRoots, result, [])
    }
  }

  let schema_to_links = (schema: Schema.t) =>
    switch schema {
    | Schema.Representation(r) =>
      [
        r.tokens->List.map(t => (r.id, t.id, ModelLink.Kind.Hierarchy)),
        r.dimensions->List.map(d => (r.id, d.id, ModelLink.Kind.Hierarchy)),
        r.schemes->List.map(s => (r.id, s.id, ModelLink.Kind.Hierarchy)),
        r.subrepresentations->List.map(r' => (r.id, r'.id, ModelLink.Kind.Hierarchy)),
      ]->Array.flatMap(List.toArray)
    | Schema.Scheme(s) =>
      [
        s.tokens->List.map(t => (s.id, t.id, ModelLink.Kind.Hierarchy)),
        s.dimensions
        ->Non_empty_list.map(d => (s.id, d.id, ModelLink.Kind.Hierarchy))
        ->Non_empty_list.toList,
        s.schemes->List.map(s' => (s.id, s'.id, ModelLink.Kind.Hierarchy)),
      ]->Array.flatMap(List.toArray)
    | Schema.Dimension(d) =>
      [
        d.dimensions->List.map(d' => (d.id, d'.id, ModelLink.Kind.Hierarchy)),
        d.tokens
        ->Non_empty_list.map(t => (d.id, t.id, ModelLink.Kind.Hierarchy))
        ->Non_empty_list.toList,
      ]->Array.flatMap(List.toArray)
    | Schema.Token(t) =>
      [
        t.sub_tokens->List.map(t' => (t.id, t'.id, ModelLink.Kind.Hierarchy)),
        t.anchored_tokens->List.map(ta => (t.id, ta.id, ModelLink.Kind.Anchor)),
        t.anchored_dimensions->List.map(da => (t.id, da.id, ModelLink.Kind.Anchor)),
        t.anchored_schemes->List.map(sa => (t.id, sa.id, ModelLink.Kind.Anchor)),
      ]->Array.flatMap(List.toArray)
    }

  let filter = (below, schemas, f) =>
    below
    ->Array.map(id =>
      schemas
      ->Gid.Map.get(id)
      ->(r =>
        switch r {
        | Some(None) => Result.Error(([], [])) // We tried to build it in the past and failed
        | None =>
          Result.Error((
            [
              internalError(
                "MISSING",
                "Expected node with ID " ++ Gid.toString(id) ++ " is missing",
              ),
            ],
            [],
          )) // Straight-up missing - eek!
        | Some(Some(r)) => Result.Ok(r)
        })
      ->Result.flatMap(f)
    )
    ->Result.all(combineMessages)
    ->Result.map(List.fromArray)

  let representation_to_slots = (
    rep: Schema.Representation.t,
  ) => InspectorState.Schema.Representation({
    InspectorState.Representation.domain: rep.domain,
    display: rep.display,
    notes: "Automatically converted from verified model.",
  })

  let slots_to_representation = (id, slots: InspectorState.Representation.t, schemas, below) => {
    let filter = f => filter(below, schemas, f)
    let domain = switch slots.domain {
    | "#Rep#" =>
      Result.Error((
        [],
        [
          ModelWarning.create(
            ~nodes=[id],
            ~message="Representation is using default domain.",
            ~details="We give each Representation the default name \"#Rep#\", but this is intended only as a placeholder.",
            ~suggestion="Replace this Representation schema's domain.",
            (),
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let display = switch slots.display {
    | "#Ref#" => Result.Error(([], [defaultReferenceWarning([id], "display", "Representation")]))
    | s => Result.Ok(s)
    }
    let tokens = filter(t =>
      switch t {
      | Schema.Token(t) => Result.Ok(t)
      | _ => Result.Error(([], []))
      }
    )
    let dimensions = filter(d =>
      switch d {
      | Schema.Dimension(d) => Result.Ok(d)
      | _ => Result.Error(([], []))
      }
    )
    let schemes = filter(s =>
      switch s {
      | Schema.Scheme(s) => Result.Ok(s)
      | _ => Result.Error(([], []))
      }
    )
    let subrepresentations = filter(r =>
      switch r {
      | Schema.Representation(r) => Result.Ok(r)
      | _ => Result.Error(([], []))
      }
    )
    (domain, display, tokens, dimensions, schemes, subrepresentations)
    ->Result.both6(combineMessages)
    ->Result.map(((
      domain,
      display,
      tokens,
      dimensions,
      schemes,
      subrepresentations,
    )) => Schema.Representation({
      Schema.Representation.id: id,
      domain: domain,
      display: display,
      tokens: tokens,
      dimensions: dimensions,
      schemes: schemes,
      subrepresentations: subrepresentations,
    }))
  }

  let scheme_to_slots = (sch: Schema.Scheme.t) => InspectorState.Schema.Scheme({
    InspectorState.Scheme.concept_structure: sch.concept_structure,
    graphic_structure: sch.graphic_structure->Option.getWithDefault("#"),
    function: Some(sch.function),
    explicit: Some(sch.explicit),
    scope: Some(sch.scope),
    organisation: sch.organisation,
    notes: "",
  })

  let slots_to_scheme = (id, slots: InspectorState.Scheme.t, schemas, below) => {
    let filter = f => filter(below, schemas, f)
    let concept_structure = switch slots.concept_structure {
    | "#Sch#" =>
      Result.Error((
        [],
        [
          ModelWarning.create(
            ~nodes=[id],
            ~message="R-Scheme is using default concept structure.",
            ~details="We give each R-Scheme the default concept structure \"#Sch#\", but this is intended only as a placeholder.",
            ~suggestion="Replace this R-Scheme schema's concept structure.",
            (),
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let graphic_structure = switch slots.graphic_structure {
    | "#Ref#" =>
      Result.Error(([], [defaultReferenceWarning([id], "graphic structure", "R-scheme")]))
    | s => Result.Ok(s)
    }
    let function = slots.function->Result.fromOption(() => ([noFunctionError([id])], []))
    let explicit = slots.explicit->Result.fromOption(() => ([noExplicitError([id])], []))
    let scope = slots.scope->Result.fromOption(() => ([noScopeError([id])], []))
    let organisation = Result.Ok(slots.organisation)
    let tokens = filter(t =>
      switch t {
      | Schema.Token(t) => Result.Ok(t)
      | _ => Result.Error(([], []))
      }
    )
    let dimensions = filter(d =>
      switch d {
      | Schema.Dimension(d) => Result.Ok(d)
      | _ => Result.Error(([], []))
      }
    )->Result.flatMap(l =>
      l
      ->Non_empty_list.fromList
      ->Result.fromOption(() => (
        [
          ModelError.create(
            ~nodes=[id],
            ~message="R-scheme has no R-dimension below it.",
            ~details="This R-scheme has no immediate children that are R-dimensions. However, an R-scheme must directly encapsulate at least one R-dimension.",
            ~suggestion="Add an R-dimension, or replace this R-scheme with another schema.",
            (),
          ),
        ],
        [],
      ))
    )
    let schemes = filter(s =>
      switch s {
      | Schema.Scheme(s) => Result.Ok(s)
      | _ => Result.Error(([], []))
      }
    )
    (
      concept_structure,
      graphic_structure,
      function,
      explicit,
      scope,
      tokens,
      dimensions,
      schemes,
      organisation,
    )
    ->Result.both9(combineMessages)
    ->Result.map(((
      concept_structure,
      graphic_structure,
      function,
      explicit,
      scope,
      tokens,
      dimensions,
      schemes,
      organisation,
    )) => Schema.Scheme({
      Schema.Scheme.id: id,
      concept_structure: concept_structure,
      graphic_structure: Some(graphic_structure),
      function: function,
      explicit: explicit,
      scope: scope,
      tokens: tokens,
      dimensions: dimensions,
      schemes: schemes,
      organisation: organisation,
    }))
  }

  let dimension_to_slots = (dim: Schema.Dimension.t) => InspectorState.Schema.Dimension({
    InspectorState.Dimension.concept: dim.concept,
    concept_scale: Some(dim.concept_scale),
    concept_attributes: dim.concept_attributes,
    graphic: dim.graphic->Option.getWithDefault("#"),
    graphic_scale: Some(dim.graphic_scale),
    graphic_attributes: dim.graphic_attributes,
    function: Some(dim.function),
    scope: Some(dim.scope),
    explicit: Some(dim.explicit),
    organisation: dim.organisation,
    notes: "",
  })

  let slots_to_dimension = (id, slots: InspectorState.Dimension.t, schemas, below) => {
    let filter = f => filter(below, schemas, f)
    let concept = switch slots.concept {
    | "#Dim#" =>
      Result.Error((
        [],
        [
          ModelWarning.create(
            ~nodes=[id],
            ~message="R-dimension is using default concept.",
            ~details="We give each R-dimension the default concept \"#Dim#\", but this is intended only as a placeholder.",
            ~suggestion="Replace this R-dimension schema's concept.",
            (),
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let concept_scale =
      slots.concept_scale->Result.fromOption(() => ([noQuantityScaleError([id], "Concept")], []))
    let concept_attributes = Result.Ok(slots.concept_attributes)
    let graphic = switch slots.graphic {
    | "#Ref#" => Result.Error(([], [defaultReferenceWarning([id], "graphic", "R-dimension")]))
    | s => Result.Ok(s)
    }
    let graphic_scale =
      slots.concept_scale->Result.fromOption(() => ([noQuantityScaleError([id], "Graphic")], []))
    let graphic_attributes = Result.Ok(slots.graphic_attributes)
    let function = slots.function->Result.fromOption(() => ([noFunctionError([id])], []))
    let explicit = slots.explicit->Result.fromOption(() => ([noExplicitError([id])], []))
    let scope = slots.scope->Result.fromOption(() => ([noScopeError([id])], []))
    let organisation = Result.Ok(slots.organisation)
    let tokens = filter(t =>
      switch t {
      | Schema.Token(t) => Result.Ok(t)
      | _ => Result.Error(([], []))
      }
    )->Result.flatMap(l =>
      l
      ->Non_empty_list.fromList
      ->Result.fromOption(() => (
        [
          ModelError.create(
            ~nodes=[id],
            ~message="R-dimension has no Tokens below it.",
            ~details="This R-dimension has no immediate children that are Tokens. However, an R-dimension must directly encapsulate at least one Token.",
            ~suggestion="Add a Token, or replace this R-dimension with another schema.",
            (),
          ),
        ],
        [],
      ))
    )
    let dimensions = filter(d =>
      switch d {
      | Schema.Dimension(d) => Result.Ok(d)
      | _ => Result.Error(([], []))
      }
    )
    let result_both12 = ((a, b, c, d, e, f, g, h, i, j, k, l), combine) =>
      Result.both(
        (Result.both6((a, b, c, d, e, f), combine), Result.both6((g, h, i, j, k, l), combine)),
        combine,
      )->Result.map((((a, b, c, d, e, f), (g, h, i, j, k, l))) => (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
      ))
    (
      concept,
      concept_scale,
      concept_attributes,
      graphic,
      graphic_scale,
      graphic_attributes,
      function,
      explicit,
      scope,
      tokens,
      dimensions,
      organisation,
    )
    ->result_both12(combineMessages)
    ->Result.map(((
      concept,
      concept_scale,
      concept_attributes,
      graphic,
      graphic_scale,
      graphic_attributes,
      function,
      explicit,
      scope,
      tokens,
      dimensions,
      organisation,
    )) => Schema.Dimension({
      Schema.Dimension.id: id,
      concept: concept,
      concept_scale: concept_scale,
      concept_attributes: concept_attributes,
      graphic: Some(graphic),
      graphic_scale: graphic_scale,
      graphic_attributes: graphic_attributes,
      function: function,
      explicit: explicit,
      scope: scope,
      tokens: tokens,
      dimensions: dimensions,
      organisation: organisation,
    }))
  }

  let token_to_slots = (tok: Schema.Token.t) => InspectorState.Schema.Token({
    InspectorState.Token.concept: tok.concept,
    graphic: tok.graphic->Option.getWithDefault("#"),
    is_class: Some(tok.is_class),
    function: Some(tok.function),
    explicit: Some(tok.explicit),
    notes: "",
  })

  let slots_to_token = (id, slots: InspectorState.Token.t, schemas, below, anchored) => {
    let filterAnchored = f => filter(anchored, schemas, f)
    let filter = f => filter(below, schemas, f)
    let concept = switch slots.concept {
    | "#Tok#" =>
      Result.Error((
        [],
        [
          ModelWarning.create(
            ~nodes=[id],
            ~message="Token is using default concept.",
            ~details="We give each Token the default concept \"#Tok#\", but this is intended only as a placeholder.",
            ~suggestion="Replace this Token schema's concept.",
            (),
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let graphic = switch slots.graphic {
    | "#Ref#" => Result.Error(([], [defaultReferenceWarning([id], "graphic", "Token")]))
    | s => Result.Ok(s)
    }
    let is_class =
      slots.is_class->Result.fromOption(() => (
        [
          ModelError.create(
            ~nodes=[id],
            ~message="Unspecified if Token is a \"class\" Token.",
            ~details="Tokens can be \"standard\" or \"class\" – that is, they can stand in for more than one actual thing in the representation. This has not been specified.",
            ~suggestion="Select whether this token is a class using the \"Is class?\" dropdown.",
            (),
          ),
        ],
        [],
      ))
    let function = slots.function->Result.fromOption(() => ([noFunctionError([id])], []))
    let explicit = slots.explicit->Result.fromOption(() => ([noExplicitError([id])], []))
    let sub_tokens = filter(t =>
      switch t {
      | Schema.Token(t) => Result.Ok(t)
      | _ =>
        Result.Error((
          [
            ModelError.create(
              ~nodes=[id, Schema.id(t)],
              ~message="All non-Token schema below a token must be anchored.",
              ~details="Children of Tokens must either be a sub-Token, or it must be an anchored schema. This schema is not anchored, but it is not a sub-Token.",
              ~suggestion="Replace this connection with an anchoring connection.",
              (),
            ),
          ],
          [],
        ))
      }
    )
    let anchored_tokens = filterAnchored(t =>
      switch t {
      | Schema.Token(t) => Result.Ok(t)
      | _ => Result.Error(([], []))
      }
    )
    let anchored_dimensions = filterAnchored(d =>
      switch d {
      | Schema.Dimension(d) => Result.Ok(d)
      | _ => Result.Error(([], []))
      }
    )
    let anchored_schemes = filterAnchored(s =>
      switch s {
      | Schema.Scheme(s) => Result.Ok(s)
      | _ => Result.Error(([], []))
      }
    )
    (
      concept,
      graphic,
      is_class,
      function,
      explicit,
      sub_tokens,
      anchored_tokens,
      anchored_dimensions,
      anchored_schemes,
    )
    ->Result.both9(combineMessages)
    ->Result.map(((
      concept,
      graphic,
      is_class,
      function,
      explicit,
      sub_tokens,
      anchored_tokens,
      anchored_dimensions,
      anchored_schemes,
    )) => Schema.Token({
      Schema.Token.id: id,
      concept: concept,
      graphic: Some(graphic),
      is_class: is_class,
      function: function,
      explicit: explicit,
      sub_tokens: sub_tokens,
      anchored_tokens: anchored_tokens,
      anchored_dimensions: anchored_dimensions,
      anchored_schemes: anchored_schemes,
    }))
  }
}

type t = {
  root: Schema.t,
  relations: array<SchemaRelation.t>,
}

let root = t => t.root
let schemas = t => {
  let schemas = []
  let rec f = schema => {
    schemas->Js.Array2.push(schema)->ignore
    Schema.children(schema)->List.forEach(f)
  }
  f(t.root)
  Array.dedup(schemas)
}
let relations = t => t.relations
let validate = t =>
  switch t.root {
  | Schema.Representation(_) => Schema.validate(t.root)
  | Schema.Scheme(_) | Schema.Dimension(_) | Schema.Token(_) =>
    Or_error.both((
      Schema.validate(t.root),
      Or_error.error_s("Model root must be a Representation schema."),
    ))->Or_error.map(((s, _)) => s)
  }

let fromSlotsAndLinks = (slots, links) => {
  let (roots, order, errors) = Conv.toposort(links, Gid.Map.keys(slots))
  switch errors {
  | [] => {
      // Great! Carry on.
      let (errs, warns) = ([], [])
      let rec f = (schemas, i) => {
        if i >= 0 {
          let id = order->Array.getExn(i)
          let children = links->Array.keepMap(((s, t, k)) =>
            if s === id && k === ModelLink.Kind.Hierarchy {
              Some(t)
            } else {
              None
            }
          )
          let anchoredChildren = links->Array.keepMap(((s, t, k)) =>
            if s === id && k === ModelLink.Kind.Anchor {
              Some(t)
            } else {
              None
            }
          )
          let s = switch slots->Gid.Map.get(id) {
          | Some(InspectorState.Schema.Representation(r)) =>
            Conv.slots_to_representation(id, r, schemas, children)
          | Some(InspectorState.Schema.Scheme(s)) => Conv.slots_to_scheme(id, s, schemas, children)
          | Some(InspectorState.Schema.Dimension(d)) =>
            Conv.slots_to_dimension(id, d, schemas, children)
          | Some(InspectorState.Schema.Token(t)) =>
            Conv.slots_to_token(id, t, schemas, children, anchoredChildren)
          | Some(InspectorState.Schema.Placeholder(_)) =>
            Result.Error((
              [],
              [
                ModelWarning.create(
                  ~nodes=[id],
                  ~message="Placeholder",
                  ~details="The Intelligence Engine cannot understand placeholders yet.",
                  (),
                ),
              ],
            ))
          | None =>
            Result.Error((
              [Conv.internalError("BAD LINK", "Node with ID " ++ Gid.toString(id) ++ " missing.")],
              [],
            ))
          }
          s
          ->Result.mapError(((e, w)) => {
            Js.Array2.pushMany(errs, e)->ignore
            Js.Array2.pushMany(warns, w)->ignore
          })
          ->ignore
          let schemas = schemas->Gid.Map.set(id, Result.toOption(s))
          f(schemas, i - 1)
        } else {
          schemas
        }
      }
      let schemas = f(Gid.Map.empty(), Array.length(order) - 1)
      let model = switch roots {
      | [] =>
        if Gid.Map.isEmpty(slots) {
          None // No model, but it's OK because it's empty!
        } else {
          let e = ModelError.create(
            ~nodes=[],
            ~message="Could not determine root of model.",
            ~details="Each model should have a root. Somehow, we failed to find a root!",
            (),
          )
          errs->Js.Array2.push(e)->ignore
          None
        }
      | [root] =>
        switch schemas->Gid.Map.get(root) {
        | None => {
            let e = Conv.internalError(
              "LOST ROOT",
              "We know what the root should be (" ++
              Gid.toString(root) ++ "), but somehow never found it!",
            )
            errs->Js.Array2.push(e)->ignore
            None
          }
        | Some(None) =>
          // Failed to build for other reasons.
          switch slots->Gid.Map.get(root) {
          | None => {
              let e = Conv.internalError(
                "EXTRA ROOT",
                "We have found an ID (" ++
                Gid.toString(root) ++ "), but have no idea what it's for!",
              )
              errs->Js.Array2.push(e)->ignore
              None
            }
          | Some(slots) =>
            // Check if it should be the root or not
            switch slots {
            | InspectorState.Schema.Placeholder(_)
            | InspectorState.Schema.Representation(_) =>
              None
            | _ => {
                let e = Conv.needsParentError([root])
                errs->Js.Array2.push(e)->ignore
                None
              }
            }
          }
        | Some(Some(r)) =>
          switch r {
          | Schema.Representation(_) => {
              let relations = links->Array.keepMap(((s, t, k)) =>
                if k === ModelLink.Kind.Relation {
                  switch (schemas->Gid.Map.get(s), schemas->Gid.Map.get(t)) {
                  | (Some(Some(s)), Some(Some(t))) =>
                    Some(
                      SchemaRelation.create(
                        s,
                        t,
                        SchemaRelation.Kind.Other("Automatic conversion"),
                      ),
                    )
                  | (Some(None), Some(None)) => None // Failed to build them, but we did find them.
                  | _ => {
                      let e = Conv.internalError(
                        "REL MISSING",
                        "A relation link should be between two nodes (" ++
                        Gid.toString(s) ++
                        ", " ++
                        Gid.toString(t) ++ "), but I can't find them!",
                      )
                      errs->Js.Array2.push(e)->ignore
                      None
                    }
                  }
                } else {
                  None
                }
              )
              Some({root: r, relations: relations})
            }
          | Schema.Scheme(_) | Schema.Dimension(_) | Schema.Token(_) => {
              let e = Conv.needsParentError([root])
              errs->Js.Array2.push(e)->ignore
              None
            }
          }
        }
      | _ => {
          roots->Array.forEach(root =>
            switch schemas->Gid.Map.get(root) {
            | None => {
                let e = Conv.internalError(
                  "LOST ROOT_MULTI",
                  "We know what this root should be (" ++
                  Gid.toString(root) ++ "), but somehow never found it!",
                )
                errs->Js.Array2.push(e)->ignore
              }
            | Some(None) =>
              // Failed to build for some other reason
              switch slots->Gid.Map.get(root) {
              | None => {
                  let e = Conv.internalError(
                    "EXTRA ROOT_MULTI",
                    "We have found an ID (" ++
                    Gid.toString(root) ++ "), but have no idea what it's for!",
                  )
                  errs->Js.Array2.push(e)->ignore
                }
              | Some(slots) =>
                switch slots {
                | InspectorState.Schema.Placeholder(_)
                | InspectorState.Schema.Representation(_) => ()
                | _ => {
                    let e = Conv.needsParentError([root])
                    errs->Js.Array2.push(e)->ignore
                  }
                }
              }
            | Some(Some(_)) => () // It went fine, but it's just one of many.
            }
          )
          let w = ModelWarning.create(
            ~nodes=roots,
            ~message="More than one model detected.",
            ~details="There are multiple models in this file, or a model with multiple roots.",
            (),
          )
          warns->Js.Array2.push(w)->ignore
          None
        }
      }
      errs->Js.Array2.reverseInPlace->ignore
      warns->Js.Array2.reverseInPlace->ignore
      model->Result.fromOption(() => (errs, warns))
    }
  | _ =>
    // Do a best-effort check, but it will be a bit crude.
    Result.Error((errors, []))
  }
}

let toSlotsAndLinks = t => {
  let schemas = schemas(t)
  let slots =
    schemas
    ->Array.map(schema =>
      switch schema {
      | Schema.Representation(r) => (r.id, Conv.representation_to_slots(r))
      | Schema.Scheme(s) => (s.id, Conv.scheme_to_slots(s))
      | Schema.Dimension(d) => (d.id, Conv.dimension_to_slots(d))
      | Schema.Token(t) => (t.id, Conv.token_to_slots(t))
      }
    )
    ->Gid.Map.fromArray
  let relations = t.relations->Array.map(r => {
    let (a, b) = SchemaRelation.schema(r)
    (Schema.id(a), Schema.id(b), ModelLink.Kind.Relation)
  })
  let links = schemas->Array.flatMap(Conv.schema_to_links)
  (slots, Array.concat(links, relations))
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("root", t.root->Schema.toJson),
    ("relations", t.relations->Array.toJson(SchemaRelation.toJson)),
  })->Js.Json.object_

let fromJson = json =>
  Js.Json.decodeObject(json)
  ->Or_error.fromOption_s("JSON is not a valid object (reading Model.t)")
  ->Or_error.flatMap(dict => {
    let root =
      dict
      ->Js.Dict.get("root")
      ->Or_error.fromOption_s("Unable to find model root (reading Model.t)")
      ->Or_error.flatMap(Schema.fromJson)
    let relations =
      dict
      ->Js.Dict.get("relations")
      ->Or_error.fromOption_s("Unable to find model relations (reading Model.t)")
      ->Or_error.flatMap(l =>
        l->Array.fromJson(j => root->Or_error.flatMap(root => j->SchemaRelation.fromJson(root)))
      )

    Or_error.both((root, relations))->Or_error.map(((root, relations)) => {
      root: root,
      relations: relations,
    })
  })
