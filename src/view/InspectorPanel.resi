@react.component
let make: (
  ~id: string,
  ~data: InspectorState.t,
  ~onChange: InspectorEvent.t => unit=?,
) => React.element
