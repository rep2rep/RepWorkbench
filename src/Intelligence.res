module T = Intelligence_Intf.WorkerThread

T.create(request => {
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
        (linkKind === ModelLink.Kind.Hierarchy || linkKind === ModelLink.Kind.Anchor) &&
          target === id
      )
      ->Option.isNone
    ) {
      errors
      ->Js.Array2.push(
        ModelError.create(
          ~node=id,
          ~message="Schema has no parent, but it needs one!",
          ~details={
            "All schemas, except for Representation schemas, must either be in the hierarchy, or be anchored below a Token in the hierarchy. This schema is neither in the hierarchy, nor anchored."
          },
        ),
      )
      ->ignore
    }
  })

  {id: request.id, errors: errors, warnings: warnings}
})
