module T = Intelligence_Intf.WorkerThread

T.create(request => {
  Js.Console.log(("Received model!", request))

  let slots = request.slots
  let links = request.links

  let errors = []
  let warnings = []

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
        linkKind === ModelLink.Kind.Hierarchy && target === id
      )
      ->Option.isNone
    ) {
      errors
      ->Js.Array2.push(ModelError.create(id, "This schema has no parent, but it needs one!"))
      ->ignore
    }
    if (
      kind === ModelNode.Kind.Representation &&
        links
        ->Array.find(((_, target, linkKind)) =>
          linkKind === ModelLink.Kind.Hierarchy && target === id
        )
        ->Option.isSome
    ) {
      warnings
      ->Js.Array2.push(
        ModelWarning.create(id, "This Representation schema has a parent - is this intensional?"),
      )
      ->ignore
    }
  })

  {errors: errors, warnings: warnings}
})
