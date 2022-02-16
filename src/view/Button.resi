module Separator: {
  @react.component
  let make: unit => React.element
}

@react.component
let make: (
  ~onClick: ReactEvent.Mouse.t => unit,
  ~style: ReactDOM.Style.t=?,
  ~value: string,
  ~enabled: bool=?,
) => React.element
