module T = Intelligence_Intf.WorkerThread

T.create(request => {
  let slots = request.slots
  let links = request.links

  let errors = []
  let warnings = []

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

  {id: request.id, errors: errors, warnings: warnings}
})
