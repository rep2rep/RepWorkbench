type t = Update(Uuid.t, InspectorEvent.t)

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
  }
