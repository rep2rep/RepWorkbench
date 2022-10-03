module T = Intelligence_Intf.WorkerThread

let removeSubsumed = (arr, subsumes) => {
  let keep = ref([])
  arr->Array.forEach(v => {
    let subsumed = ref(false)
    keep :=
      keep.contents->Array.mapPartial(v' => {
        let s = subsumes(v', v)
        let s' = subsumes(v, v')
        switch (s, s') {
        | (true, true) | (true, false) => {
            subsumed := true
            Some(v')
          }
        | (false, true) => None
        | (false, false) => Some(v')
        }
      })
    if !subsumed.contents {
      keep.contents->Js.Array2.push(v)->ignore
    }
  })
  keep.contents
}

T.listen(request => {
  let slots = request.slots
  let links = request.links

  let errors = []
  let warnings = []
  let insights = []

  // let error = (~nodes, ~message, ~details, ~suggestion=?, ()) =>
  // errors->Js.Array2.push(ModelError.create(~nodes, ~message, ~details, ~suggestion?, ()))->ignore
  let warning = (~nodes, ~message, ~details, ~suggestion=?, ()) =>
    warnings
    ->Js.Array2.push(ModelWarning.create(~nodes, ~message, ~details, ~suggestion?, ()))
    ->ignore

  let model = Model.fromSlotsAndLinks(slots, links)
  model
  ->Result.mapError(((errs, warns)) => {
    Js.Array2.pushMany(errors, errs)->ignore
    Js.Array2.pushMany(warnings, warns)->ignore
  })
  ->ignore

  links->Array.forEach(((sa, ta, ka) as a) =>
    links->Array.forEach(((sb, tb, kb) as b) => {
      if (
        a !== b && ta === sb && ka === ModelLink.Kind.Hierarchy && kb === ModelLink.Kind.Hierarchy
      ) {
        let c = links->Array.find(((sc, tc, kc) as c) => {
          sa === sc && tb === tc && kc === ModelLink.Kind.Hierarchy && a !== c && b !== c
        })
        if Option.isSome(c) {
          warning(
            ~nodes=[sa, ta, tb],
            ~message="Transitive hierarchical connections",
            ~details="Connections in the hierarchy are transitive, so normally you do not connect a grandparent schema to its grandchildren schemas.",
            ~suggestion="Remove the transitive connection.",
            (),
          )
        }
      }
    })
  )

  // Annoyingly, errors and warnings might get duplicated due to parallel connections.
  // We have to remove them, or else we will get problems!
  let errors = Array.dedup(errors)
  let warnings = Array.dedup(warnings)

  T.respond({
    id: request.id,
    model: request.model,
    errors: errors,
    errors_done: true,
    warnings: warnings,
    warnings_done: true,
    insights: [],
    insights_done: false,
    killed: false,
  })

  let equiv_schemas = (slots, kind) => {
    switch (slots, kind) {
    | (InspectorState.Schema.Representation(_), Idiom.Node.Representation)
    | (InspectorState.Schema.Scheme(_), Idiom.Node.Scheme)
    | (InspectorState.Schema.Dimension(_), Idiom.Node.Dimension)
    | (InspectorState.Schema.Placeholder(_), Idiom.Node.Placeholder) => true
    | (InspectorState.Schema.Token(t), Idiom.Node.Token(t')) =>
      t.is_class->Option.getWithDefault(false) === t'.is_class
    | (_, Idiom.Node.Any) => true
    | _ => false
    }
  }
  let equiv_links = (k1, k2) =>
    switch (k1, k2) {
    | (ModelLink.Kind.Hierarchy, Idiom.Link.Hierarchy)
    | (ModelLink.Kind.Anchor, Idiom.Link.Anchor)
    | (ModelLink.Kind.Generic, Idiom.Link.Generic) => true
    | (_, Idiom.Link.Any) => true
    | _ => false
    }
  let isSubsumed = ins => insights->Array.some(i => ModelInsight.subsumes(i, ins))

  let idiom = (idiom, insight) =>
    SubgraphIsomorphism.findIsomorphism(
      ~whole=(slots, links),
      ~find=idiom,
      ~equiv_schemas,
      ~equiv_links,
      ~onFind={
        ((schs, _)) => {
          let nodes = Gid.Map.keys(schs)
          nodes->Belt.SortArray.stableSortInPlaceBy(Gid.compare)
          let ins = insight(~nodes, ())
          if !isSubsumed(ins) {
            insights->Js.Array2.push(ins)->ignore
            T.respond({
              id: request.id,
              model: request.model,
              errors: errors,
              errors_done: true,
              warnings: warnings,
              warnings_done: true,
              insights: insights,
              insights_done: false,
              killed: false,
            })
          }
        }
      },
    )

  idiom(Idiom.pickCollection, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Collection Pick idiom detected.",
      ~details="The Collection Pick idiom selects one or more particular tokens out from a class token, because they are worth identifying in some way.",
      (),
    )
  )

  idiom(Idiom.filterCollection, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Collection Filter idiom detected.",
      ~details="The Collection Filter idiom refines a collection from everything, down to just the part of the collection begin interpreted.",
      (),
    )
  )

  idiom(Idiom.forEachCollection, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Collection For-each idiom detected.",
      ~details="The Collection For-each idiom describes an interpretation that is true for every element in the collection.",
      (),
    )
  )

  idiom(Idiom.reduceCollection, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Collection Reduce idiom detected.",
      ~details="The Collection Reduce idiom abstracts a collection up to a single R-Symbol standing for the entire collection at once.",
      (),
    )
  )

  idiom(Idiom.sumDimension, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Sum R-dimension idiom detected.",
      ~details="The Sum R-dimension idiom indicates that the parent R-dimension can be considered as being 'made up of' the children (sub) R-dimensions.",
      (),
    )
  )

  idiom(Idiom.prodDimension, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Product R-dimension idiom detected.",
      ~details="The Product R-dimension idiom indicates that the child R-dimension can be considered as being 'a combination of' the parent R-dimensions.",
      (),
    )
  )

  idiom(Idiom.explicitCoordinateSystem, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Explicit Coordinate System idiom detected.",
      ~details="The Explicit Coordinate System idiom indicates the R-dimensions together form the coordinate system indicated by the upper R-scheme, with the 'data' R-dimensions attached to the upper R-Scheme, and the lower R-scheme combining the 'coordinate' R-dimensions.",
      (),
    )
  )

  idiom(Idiom.implicitCoordinateSystem, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Implicit Coordinate System idiom detected.",
      ~details="The Implicit Coordinate System idiom indicates the R-dimensions together form the coordinate system indicated by the R-scheme, but there is no distinction between which are the 'data' and which are the 'coordinates'.",
      (),
    )
  )

  let insights = Array.dedup(insights)

  T.respond({
    id: request.id,
    model: request.model,
    errors: errors,
    errors_done: true,
    warnings: warnings,
    warnings_done: true,
    insights: insights,
    insights_done: true,
    killed: false,
  })
})
