module T = Intelligence_Intf.WorkerThread

T.create(request => {
  let slots = request.slots
  let links = request.links

  let errors = []
  let warnings = []

  let error = (~nodes, ~message, ~details, ~suggestion=?, ()) =>
    errors->Js.Array2.push(ModelError.create(~nodes, ~message, ~details, ~suggestion?, ()))->ignore
  let warning = (~nodes, ~message, ~details, ~suggestion=?, ()) =>
    warnings
    ->Js.Array2.push(ModelWarning.create(~nodes, ~message, ~details, ~suggestion?, ()))
    ->ignore

  slots->Gid.Map.forEach((id, slots) => {
    let kind = switch slots {
    | InspectorState.Schema.Representation(_) => ModelNode.Kind.Representation
    | InspectorState.Schema.Scheme(_) => ModelNode.Kind.Scheme
    | InspectorState.Schema.Dimension(_) => ModelNode.Kind.Dimension
    | InspectorState.Schema.Token(_) => ModelNode.Kind.Token
    | InspectorState.Schema.Placeholder(_) => ModelNode.Kind.Placeholder
    }
    if (
      kind !== ModelNode.Kind.Representation &&
      kind !== ModelNode.Kind.Placeholder &&
      links
      ->Array.find(((_, target, linkKind)) =>
        (linkKind === ModelLink.Kind.Hierarchy || linkKind === ModelLink.Kind.Anchor) &&
          target === id
      )
      ->Option.isNone
    ) {
      error(
        ~nodes=[id],
        ~message="Schema has no parent, but it needs one.",
        ~details="All schemas, except for Representation schemas, must either be in the hierarchy, or be anchored below a Token in the hierarchy. This schema is neither in the hierarchy, nor anchored.",
        ~suggestion="Connect this schema below another schema.",
        (),
      )
    }
  })

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

  {id: request.id, errors: errors, warnings: warnings}
})
