type t =
  | UpdateSchema(Uuid.t, InspectorEvent.t)
  | DuplicateSchema(Uuid.Map.t<Uuid.t>)
  | UpdateGlobal(InspectorEvent.t)

let dispatch = (state, t) =>
  switch t {
  | UpdateSchema(key, event) =>
    switch state {
    | InspectorState.Single(key', s) =>
      InspectorState.Single(
        key,
        if key === key' {
          InspectorState.Schema.applyEvent(s, event)
        } else {
          s
        },
      )
    | s => s
    }
  | DuplicateSchema(keyMap) => {
      let dup = (source, slots) =>
        keyMap->Uuid.Map.get(source)->Option.map(target => (target, slots))
      switch state {
      | InspectorState.Single(source, s) =>
        dup(source, s)
        ->Option.map(((k, v)) => InspectorState.Single(k, v))
        ->Option.getWithDefault(InspectorState.Single(source, s))
      | InspectorState.Multiple(ss) =>
        ss->Array.mapPartial(((source, s)) => dup(source, s))->InspectorState.Multiple
      | s => s
      }
    }
  | _ => state
  }
