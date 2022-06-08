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

  T.respond({
    id: request.id,
    errors: errors,
    errors_done: true,
    warnings: warnings,
    warnings_done: true,
    insights: [],
    insights_done: false,
  })

  let equiv_schemas = (slots, kind) => {
    switch (slots, kind) {
    | (InspectorState.Schema.Representation(_), ModelNode.Kind.Representation)
    | (InspectorState.Schema.Scheme(_), ModelNode.Kind.Scheme)
    | (InspectorState.Schema.Dimension(_), ModelNode.Kind.Dimension)
    | (InspectorState.Schema.Token(_), ModelNode.Kind.Token)
    | (InspectorState.Schema.Placeholder(_), ModelNode.Kind.Placeholder) => true
    | _ => false
    }
  }
  let equiv_links = (k1, k2) => k1 === k2

  let idiom = (values, idiom, insight) => {
    values
    ->Array.flatMap(v => {
      SubgraphIsomorphism.findIsomorphism(
        ~whole=(slots, links),
        ~find=idiom(v),
        ~equiv_schemas,
        ~equiv_links,
      )
    })
    ->Array.map(((schs, _)) => {
      let nodes = Gid.Map.keys(schs)
      nodes->Belt.SortArray.stableSortInPlaceBy(Gid.compare)
      insight(~nodes, ())
    })
    ->Array.map(ins => (ModelInsight.id(ins), ins))
    ->Gid.Map.fromArray
    ->Gid.Map.values
    ->removeSubsumed(ModelInsight.subsumes)
  }

  let sumDimensions = idiom([3, 2], Idiom.sumDimension, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Sum R-dimension idiom detected.",
      ~details="The Sum R-dimension idiom indicates that the parent R-dimension can be considered as being 'made up of' the children (sub) R-dimensions.",
      (),
    )
  )
  insights->Js.Array2.pushMany(sumDimensions)->ignore
  let prodDimensions = idiom([3, 2], Idiom.prodDimension, (~nodes, ()) =>
    ModelInsight.create(
      ~nodes,
      ~message="Product R-dimension idiom detected.",
      ~details="The Product R-dimension idiom indicates that the child R-dimension can be considered as being 'a combination of' the parent R-dimensions.",
      (),
    )
  )
  insights->Js.Array2.pushMany(prodDimensions)->ignore

  // Annoyingly, errors and warnings might get duplicated due to parallel connections.
  // We have to remove them, or else we will get problems!
  let errors = Array.dedup(errors)
  let warnings = Array.dedup(warnings)
  let insights = Array.dedup(insights)

  T.respond({
    id: request.id,
    errors: errors,
    errors_done: true,
    warnings: warnings,
    warnings_done: true,
    insights: insights,
    insights_done: true,
  })
})
