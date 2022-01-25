@react.component
let make = (~onClick, ~style as givenStyle=?, ~children) => {
  let newStyle = ReactDOM.Style.make(
    ~padding="0.2rem 0.3rem",
    ~margin="0.125rem 0.25rem",
    ~fontSize="small",
    (),
  )
  let style = switch givenStyle {
  | Some(givenStyle) => ReactDOM.Style.combine(newStyle, givenStyle)
  | None => newStyle
  }
  <button onClick style> {children} </button>
}
