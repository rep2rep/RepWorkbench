@react.component
let make: (
  ~id: string,
  ~data: InspectorState.t,
  ~onChange: Event.Model.t => unit=?,
) => React.element
