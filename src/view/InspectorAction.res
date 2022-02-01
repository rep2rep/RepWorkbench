type t =
  | Update(Uuid.t, InspectorEvent.t)
  | Duplicate(Uuid.Map.t<Uuid.t>)

let dispatch = (state, t) =>
  switch t {
  | Update(key, event) => {
      let newState = switch state {
      | InspectorState.Single(key', s) =>
        if key == key' {
          Some(InspectorState.Schema.applyEvent(s, event))
        } else {
          None
        }
      | _ => None
      }
      [(key, newState)]
    }
  | Duplicate(keyMap) => {
      let dup = (source, slots) =>
        keyMap
        ->Uuid.Map.get(source)
        ->Option.map(target => [(target, Some(slots))])
        ->Option.getWithDefault([])
      switch state {
      | InspectorState.Single(source, s) => dup(source, s)
      | InspectorState.Multiple(ss) => ss->Array.flatMap(((source, s)) => dup(source, s))
      | _ => []
      }
    }
  }
