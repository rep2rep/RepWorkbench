module Conv = {
  let combineMessages = msgs =>
    msgs->Array.reduce(([], []), ((errs, warns), (e, w)) => (
      Array.concat(errs, e),
      Array.concat(warns, w),
    ))

  let toposort = (links, all_ids) => {
    let result = []
    let links =
      links->Array.filter(((_, _, k)) =>
        k === ModelLink.Kind.Hierarchy || k == ModelLink.Kind.Anchor
      )
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
    if Array.length(firstRoots) === 0 && Array.length(all_ids) !== 0 {
      let err = ModelError.cyclesError
      (firstRoots, result, [err])
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
        s.dimensions->List.map(d => (s.id, d.id, ModelLink.Kind.Hierarchy)),
        s.schemes->List.map(s' => (s.id, s'.id, ModelLink.Kind.Hierarchy)),
      ]->Array.flatMap(List.toArray)
    | Schema.Dimension(d) =>
      [
        d.dimensions->List.map(d' => (d.id, d'.id, ModelLink.Kind.Hierarchy)),
        d.tokens->List.map(t => (d.id, t.id, ModelLink.Kind.Hierarchy)),
      ]->Array.flatMap(List.toArray)
    | Schema.Token(t) =>
      [
        t.sub_tokens->List.map(t' => (t.id, t'.id, ModelLink.Kind.Hierarchy)),
        t.anchored_tokens->List.map(ta => (t.id, ta.id, ModelLink.Kind.Anchor)),
        t.anchored_dimensions->List.map(da => (t.id, da.id, ModelLink.Kind.Anchor)),
        t.anchored_schemes->List.map(sa => (t.id, sa.id, ModelLink.Kind.Anchor)),
      ]->Array.flatMap(List.toArray)
    }

  let filter = (~allowPlaceholders=true, below, schemas, desiredKind, f) =>
    below->Array.mapPartial(id =>
      schemas
      ->Gid.Map.get(id)
      ->(
        r =>
          switch r {
          | None =>
            // Straight-up missing - eek!
            Result.Error((
              [
                ModelError.internalError(
                  ~code="MISSING",
                  ~details="Expected node with ID " ++ Gid.toString(id) ++ " is missing",
                ),
              ],
              [],
            ))->Some
          | Some((k, v)) =>
            if k === desiredKind || (k == ModelNode.Kind.Placeholder && allowPlaceholders) {
              f(id, v)
            } else {
              None
            }
          }
      )
    )

  let hasAnchorsResult = (node, anchors, schemas, kind) => {
    if Array.length(anchors) === 0 {
      Result.Ok()
    } else {
      let msgs =
        anchors
        ->Array.mapPartial(id =>
          schemas
          ->Gid.Map.get(id)
          ->Option.map(((_, v)) =>
            switch v {
            | Result.Ok(_) => Result.Ok()
            | Result.Error(e) => Result.Error(e)
            }
          )
        )
        ->Result.allUnit(combineMessages)
      let anchors = Result.Error([ModelError.unexpectedAnchorsError([node], kind)], [])
      [msgs, anchors]->Result.allUnit(combineMessages)
    }
  }

  let representation_to_slots = (
    rep: Schema.Representation.t,
  ) => InspectorState.Schema.Representation({
    InspectorState.Representation.domain: rep.domain,
    display: rep.display,
    notes: "Automatically converted from verified model.",
  })

  let slots_to_representation = (
    id,
    slots: InspectorState.Representation.t,
    schemas,
    below,
    anchored,
  ) => {
    let filter = (~allowPlaceholders=true, k, f) => filter(~allowPlaceholders, below, schemas, k, f)
    let domain = switch slots.domain {
    | "#Rep#" =>
      Result.Error((
        [],
        [
          ModelWarning.defaultConceptWarning(
            [id],
            ~field="domain",
            ~default="#Rep#",
            #representation,
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let display = switch slots.display {
    | "#Ref#" =>
      Result.Error(([], [ModelWarning.defaultReferenceWarning([id], "display", #representation)]))
    | s => Result.Ok(s)
    }
    let tokens =
      filter(ModelNode.Kind.Token, (_, t) =>
        switch t {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Token(t)) => Some(Result.Ok(t))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let dimensions =
      filter(ModelNode.Kind.Dimension, (_, d) =>
        switch d {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Dimension(d)) => Some(Result.Ok(d))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let schemes =
      filter(ModelNode.Kind.Scheme, (_, s) =>
        switch s {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Scheme(s)) => Some(Result.Ok(s))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let subrepresentations =
      filter(ModelNode.Kind.Representation, (_, r) =>
        switch r {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Representation(r)) => Some(Result.Ok(r))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let anchors__ = hasAnchorsResult(id, anchored, schemas, #representation)
    (domain, display, tokens, dimensions, schemes, subrepresentations, anchors__)
    ->Result.both7(combineMessages)
    ->Result.map(((
      domain,
      display,
      tokens,
      dimensions,
      schemes,
      subrepresentations,
      (),
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

  let slots_to_scheme = (id, slots: InspectorState.Scheme.t, schemas, below, anchored) => {
    let filter = (~allowPlaceholders=true, k, f) => filter(~allowPlaceholders, below, schemas, k, f)
    let concept_structure = switch slots.concept_structure {
    | "#Sch#" =>
      Result.Error((
        [],
        [
          ModelWarning.defaultConceptWarning(
            [id],
            ~field="concept structure",
            ~default="#Sch#",
            #scheme,
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let graphic_structure = switch slots.graphic_structure {
    | "#Ref#" =>
      Result.Error(([], [ModelWarning.defaultReferenceWarning([id], "graphic structure", #scheme)]))
    | s => Result.Ok(s)
    }
    let function = slots.function->Result.fromOption(() => ([ModelError.noFunctionError([id])], []))
    let explicit = slots.explicit->Result.fromOption(() => ([ModelError.noExplicitError([id])], []))
    let scope = slots.scope->Result.fromOption(() => ([ModelError.noScopeError([id])], []))
    let organisation = Result.Ok(slots.organisation)
    let tokens =
      filter(ModelNode.Kind.Token, (_, t) =>
        switch t {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Token(t)) => Some(Result.Ok(t))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let dimensions =
      filter(ModelNode.Kind.Dimension, (_, d) =>
        switch d {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Dimension(d)) => Some(Result.Ok(d))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let schemes =
      filter(ModelNode.Kind.Scheme, (_, s) =>
        switch s {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Scheme(s)) => Some(Result.Ok(s))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let representations__ =
      filter(~allowPlaceholders=false, ModelNode.Kind.Representation, (id', _) => Some(
        Result.Error(
          [ModelError.badHierarchyError([id, id'], ~parent=#scheme, ~child=#representation)],
          [],
        ),
      ))->Result.allUnit(combineMessages)
    let anchors__ = hasAnchorsResult(id, anchored, schemas, #scheme)

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
      representations__,
      anchors__,
    )
    ->Result.both11(combineMessages)
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
      (),
      (),
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

  let slots_to_dimension = (id, slots: InspectorState.Dimension.t, schemas, below, anchored) => {
    let filter = (~allowPlaceholders=true, k, f) => filter(~allowPlaceholders, below, schemas, k, f)
    let concept = switch slots.concept {
    | "#Dim#" =>
      Result.Error((
        [],
        [ModelWarning.defaultConceptWarning([id], ~field="concept", ~default="#Dim#", #dimension)],
      ))
    | s => Result.Ok(s)
    }
    let concept_scale =
      slots.concept_scale->Result.fromOption(() => (
        [ModelError.noQuantityScaleError([id], #concept)],
        [],
      ))
    let concept_attributes = Result.Ok(slots.concept_attributes)
    let graphic = switch slots.graphic {
    | "#Ref#" =>
      Result.Error(([], [ModelWarning.defaultReferenceWarning([id], "graphic", #dimension)]))
    | s => Result.Ok(s)
    }
    let graphic_scale =
      slots.concept_scale->Result.fromOption(() => (
        [ModelError.noQuantityScaleError([id], #graphic)],
        [],
      ))
    let graphic_attributes = Result.Ok(slots.graphic_attributes)
    let function = slots.function->Result.fromOption(() => ([ModelError.noFunctionError([id])], []))
    let explicit = slots.explicit->Result.fromOption(() => ([ModelError.noExplicitError([id])], []))
    let scope = slots.scope->Result.fromOption(() => ([ModelError.noScopeError([id])], []))
    let organisation = Result.Ok(slots.organisation)

    let tokCount = ref(0)
    let dimCount = ref(0)
    let tokens =
      filter(ModelNode.Kind.Token, (_, t) =>
        switch t {
        | Result.Error(e) => {
            tokCount := tokCount.contents + 1
            Some(Result.Error(e))
          }
        | Result.Ok(Schema.Token(t)) => {
            tokCount := tokCount.contents + 1
            Some(Result.Ok(t))
          }
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let dimensions =
      filter(ModelNode.Kind.Dimension, (_, d) =>
        switch d {
        | Result.Error(e) => {
            dimCount := dimCount.contents + 1
            Some(Result.Error(e))
          }
        | Result.Ok(Schema.Dimension(d)) => {
            dimCount := dimCount.contents + 1
            Some(Result.Ok(d))
          }
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)
    let representations__ =
      filter(~allowPlaceholders=false, ModelNode.Kind.Representation, (id', _) => Some(
        Result.Error(
          [ModelError.badHierarchyError([id, id'], ~parent=#dimension, ~child=#representation)],
          [],
        ),
      ))->Result.allUnit(combineMessages)
    let schemes__ =
      filter(~allowPlaceholders=false, ModelNode.Kind.Scheme, (id', _) => Some(
        Result.Error(
          [ModelError.badHierarchyError([id, id'], ~parent=#dimension, ~child=#scheme)],
          [],
        ),
      ))->Result.allUnit(combineMessages)
    let anchors__ = hasAnchorsResult(id, anchored, schemas, #dimension)

    let at_least_one_token_or_dimension__ = if tokCount.contents + dimCount.contents >= 1 {
      Result.Ok()
    } else {
      Result.Error(
        [
          ModelError.create(
            ~nodes=[id],
            ~message="R-dimensions must have at least one child.",
            ~details="R-dimensions must contain at least one R-symbol, or must split into sub-R-dimensions (or both). This R-dimension has no R-symbol children and no sub-R-dimensions.",
            ~suggestion="Add an R-symbol or a sub-R-dimension below this R-dimension.",
            (),
          ),
        ],
        [],
      )
    }
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
      representations__,
      schemes__,
      anchors__,
      at_least_one_token_or_dimension__,
    )
    ->Result.both16(combineMessages)
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
      (),
      (),
      (),
      (),
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
    let filterAnchored = (k, f) => filter(anchored, schemas, k, f)
    let filter = (~allowPlaceholders=true, k, f) => filter(~allowPlaceholders, below, schemas, k, f)
    let concept = switch slots.concept {
    | "#Sym#" =>
      Result.Error((
        [],
        [ModelWarning.defaultConceptWarning([id], ~field="concept", ~default="#Sym#", #token)],
      ))
    | s => Result.Ok(s)
    }
    let graphic = switch slots.graphic {
    | "#Ref#" => Result.Error(([], [ModelWarning.defaultReferenceWarning([id], "graphic", #token)]))
    | s => Result.Ok(s)
    }
    let is_class =
      slots.is_class->Result.fromOption(() => (
        [
          ModelError.create(
            ~nodes=[id],
            ~message="Unspecified if R-symbol is a \"class\" R-symbol.",
            ~details="R-symbols can be \"standard\" or \"class\": that is, they can stand in for more than one actual thing in the representation. This has not been specified.",
            ~suggestion="Select whether this token is a class using the \"Is class?\" dropdown.",
            (),
          ),
        ],
        [],
      ))
    let function = slots.function->Result.fromOption(() => ([ModelError.noFunctionError([id])], []))
    let explicit = slots.explicit->Result.fromOption(() => ([ModelError.noExplicitError([id])], []))

    let badNonAnchorError = (id', kind, anchor) =>
      ModelError.create(
        ~nodes=[id, id'],
        ~message={
          if kind->String.startsWith("R-") {
            "An "
          } else {
            "A "
          } ++
          kind ++ " schema below an R-symbol must be anchored."
        },
        ~details="Children of R-symbols must either be a sub-R-symbol, or it must be an anchored schema. This schema is not anchored, but it is not a sub-R-symbol: it is " ++
        if kind->String.startsWith("R-") {
          "an "
        } else {
          "a "
        } ++
        kind ++ ".",
        ~suggestion={
          if anchor {
            "Replace this connection with an anchoring connection."
          } else {
            "Remove this schema, or place it elsewhere in the hierarchy."
          }
        },
        (),
      )

    let sub_tokens =
      filter(ModelNode.Kind.Token, (_, t) =>
        switch t {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Token(t)) => Some(Result.Ok(t))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)

    let dimensions__ =
      filter(~allowPlaceholders=false, ModelNode.Kind.Dimension, (id', _) => Some(
        Result.Error([badNonAnchorError(id', "R-dimension", true)], []),
      ))->Result.allUnit(combineMessages)

    let schemes__ =
      filter(~allowPlaceholders=false, ModelNode.Kind.Scheme, (id', _) => Some(
        Result.Error([badNonAnchorError(id', "R-scheme", true)], []),
      ))->Result.allUnit(combineMessages)

    let representations__ =
      filter(~allowPlaceholders=false, ModelNode.Kind.Representation, (id', _) => Some(
        Result.Error([badNonAnchorError(id', "Representation", false)], []),
      ))->Result.allUnit(combineMessages)

    let anchored_tokens =
      filterAnchored(ModelNode.Kind.Token, (_, t) =>
        switch t {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Token(t)) => Some(Result.Ok(t))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)

    let anchored_dimensions =
      filterAnchored(ModelNode.Kind.Dimension, (_, d) =>
        switch d {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Dimension(d)) => Some(Result.Ok(d))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)

    let anchored_schemes =
      filterAnchored(ModelNode.Kind.Scheme, (_, s) =>
        switch s {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Scheme(s)) => Some(Result.Ok(s))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)

    let anchored_representations =
      filterAnchored(ModelNode.Kind.Representation, (_, r) =>
        switch r {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Representation(r)) => Some(Result.Ok(r))
        | _ => None
        }
      )
      ->Result.all(combineMessages)
      ->Result.map(List.fromArray)

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
      anchored_representations,
      dimensions__,
      schemes__,
      representations__,
    )
    ->Result.both13(combineMessages)
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
      anchored_representations,
      (),
      (),
      (),
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
      anchored_representations: anchored_representations,
    }))
  }

  let slots_to_placeholder = (
    id,
    slots: InspectorState.Placeholder.t,
    schemas,
    below,
    anchored,
  ) => {
    let filterAnchored = (~allowPlaceholders=true, k, f) =>
      filter(~allowPlaceholders, anchored, schemas, k, f)
    let filter = (~allowPlaceholders=true, k, f) => filter(~allowPlaceholders, below, schemas, k, f)

    let description = switch slots.description {
    | "#Placeholder#" =>
      Result.Error((
        [],
        [
          ModelWarning.defaultConceptWarning(
            [id],
            ~field="description",
            ~default="#Placeholder#",
            #placeholder,
          ),
        ],
      ))
    | s => Result.Ok(s)
    }
    let isIntensional =
      slots.isIntensional->Result.fromOption(() => (
        [
          ModelError.create(
            ~nodes=[id],
            ~message="Unspecified why Placeholder is included.",
            ~details="Placeholders can be included because the substructure is not understood, or because it is understood but has chosen not to be modelled. You must indicate which case this is.",
            ~suggestion="Select whether this Placeholder is understood using the \"Omitted but understood?\" dropdown.",
            (),
          ),
        ],
        [],
      ))

    let representations__ = filter(~allowPlaceholders=true, ModelNode.Kind.Representation, (_, t) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Representation(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    let schemes__ = filter(~allowPlaceholders=false, ModelNode.Kind.Scheme, (_, t) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Scheme(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    let dimensions__ = filter(~allowPlaceholders=false, ModelNode.Kind.Dimension, (_, t) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Dimension(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    let tokens__ = filter(~allowPlaceholders=false, ModelNode.Kind.Token, (_, t) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Token(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    let anchored_representations__ = filterAnchored(
      ~allowPlaceholders=true,
      ModelNode.Kind.Representation,
      (_, t) =>
        switch t {
        | Result.Error(e) => Some(Result.Error(e))
        | Result.Ok(Schema.Representation(_)) => Some(Result.Ok())
        | _ => None
        },
    )->Result.allUnit(combineMessages)

    let anchored_schemes__ = filterAnchored(~allowPlaceholders=false, ModelNode.Kind.Scheme, (
      _,
      t,
    ) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Scheme(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    let anchored_dimensions__ = filterAnchored(~allowPlaceholders=false, ModelNode.Kind.Dimension, (
      _,
      t,
    ) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Dimension(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    let anchored_tokens__ = filterAnchored(~allowPlaceholders=false, ModelNode.Kind.Token, (_, t) =>
      switch t {
      | Result.Error(e) => Some(Result.Error(e))
      | Result.Ok(Schema.Token(_)) => Some(Result.Ok())
      | _ => None
      }
    )->Result.allUnit(combineMessages)

    (
      description,
      isIntensional,
      representations__,
      schemes__,
      dimensions__,
      tokens__,
      anchored_representations__,
      anchored_schemes__,
      anchored_dimensions__,
      anchored_tokens__,
    )
    ->Result.both10(combineMessages)
    ->Result.flatMap(_ => Result.Error(([], [])))
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
          let (k, s) = switch slots->Gid.Map.get(id) {
          | Some(InspectorState.Schema.Representation(r)) => (
              ModelNode.Kind.Representation,
              Conv.slots_to_representation(id, r, schemas, children, anchoredChildren),
            )
          | Some(InspectorState.Schema.Scheme(s)) => (
              ModelNode.Kind.Scheme,
              Conv.slots_to_scheme(id, s, schemas, children, anchoredChildren),
            )
          | Some(InspectorState.Schema.Dimension(d)) => (
              ModelNode.Kind.Dimension,
              Conv.slots_to_dimension(id, d, schemas, children, anchoredChildren),
            )
          | Some(InspectorState.Schema.Token(t)) => (
              ModelNode.Kind.Token,
              Conv.slots_to_token(id, t, schemas, children, anchoredChildren),
            )
          | Some(InspectorState.Schema.Placeholder(p)) => (
              ModelNode.Kind.Placeholder,
              Conv.slots_to_placeholder(id, p, schemas, children, anchoredChildren), // Always returns an error.
            )
          | None => (
              ModelNode.Kind.Placeholder,
              Result.Error((
                [
                  ModelError.internalError(
                    ~code="BAD LINK",
                    ~details="Node with ID " ++ Gid.toString(id) ++ " missing.",
                  ),
                ],
                [],
              )),
            )
          }

          let schemas = schemas->Gid.Map.set(id, (k, s))
          f(schemas, i - 1)
        } else {
          schemas
        }
      }

      let schemas = f(Gid.Map.empty(), Array.length(order) - 1)

      let handleRoot = (root, ~multi) =>
        switch schemas->Gid.Map.get(root) {
        | None => {
            let e = ModelError.internalError(
              ~code="LOST ROOT" ++ if multi {
                " MULTI"
              } else {
                ""
              },
              ~details="We know what the root should be (" ++
              Gid.toString(root) ++ "), but somehow never found it!",
            )
            schemas
            ->Gid.Map.values
            ->Array.map(((_, r)) => r)
            ->Result.all(Conv.combineMessages)
            ->Result.thenError(([e], []), Conv.combineMessages)
          }
        | Some((_, Result.Error(_) as err)) =>
          // Failed to build for other reasons.
          switch slots->Gid.Map.get(root) {
          | None => {
              let e = ModelError.internalError(
                ~code="EXTRA ROOT",
                ~details="We have found an ID (" ++
                Gid.toString(root) ++ "), but have no idea what it's for!",
              )
              err->Result.thenError(([e], []), Conv.combineMessages)
            }
          | Some(slots) =>
            // Check if it should be the root or not
            switch slots {
            | InspectorState.Schema.Placeholder(_)
            | InspectorState.Schema.Representation(_) => err
            | _ => {
                let e = ModelError.needsParentError([root])
                err->Result.thenError(([e], []), Conv.combineMessages)
              }
            }
          }
        | Some((_, Result.Ok(r))) =>
          switch r {
          | Schema.Representation(_) => {
              let relations =
                links
                ->Array.keepMap(((s, t, k)) =>
                  if k === ModelLink.Kind.Generic {
                    switch (schemas->Gid.Map.get(s), schemas->Gid.Map.get(t)) {
                    | (Some((_, Result.Ok(s))), Some((_, Result.Ok(t)))) =>
                      Result.Ok(
                        SchemaRelation.create(
                          s,
                          t,
                          SchemaRelation.Kind.Other("Automatic conversion"),
                        ),
                      )->Some
                    | (Some((_, Result.Error(_))), Some((_, Result.Error(_)))) => None // Failed to build them, but we did find them.
                    | _ => {
                        let e = ModelError.internalError(
                          ~code="REL MISSING",
                          ~details="A relation link should be between two nodes (" ++
                          Gid.toString(s) ++
                          ", " ++
                          Gid.toString(t) ++ "), but I can't find them!",
                        )
                        Result.Error(([e], []))->Some
                      }
                    }
                  } else {
                    None
                  }
                )
                ->Result.all(Conv.combineMessages)
              relations->Result.map(relations => {root: r, relations: relations})
            }
          | Schema.Scheme(_) | Schema.Dimension(_) | Schema.Token(_) => {
              let e = ModelError.needsParentError([root])
              Result.Error(([e], []))
            }
          }
        }
      switch roots {
      | [] =>
        if Gid.Map.isEmpty(slots) {
          Result.Error(([], [])) // No model, but it's OK because it's empty!
        } else {
          let e = ModelError.noRootError
          Result.Error(([e], []))
        }
      | [root] => handleRoot(root, ~multi=false)
      | _ => {
          let models = roots->Array.map(handleRoot(_, ~multi=true))
          let w = ModelWarning.multipleRootsWarning(roots)
          models
          ->Result.all(Conv.combineMessages)
          ->Result.thenError(([], [w]), Conv.combineMessages)
        }
      }
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
    (Schema.id(a), Schema.id(b), ModelLink.Kind.Generic)
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
