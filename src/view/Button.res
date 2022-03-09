module Separator = {
  @react.component
  let make = () => {
    <span
      style={ReactDOM.Style.make(
        ~display="inline-block",
        ~width="1pt",
        ~height="2.5ex",
        ~margin="0 0.5em",
        ~background="#ddd",
        (),
      )}
    />
  }
}

@react.component
let make = (~onClick, ~style as givenStyle=?, ~value, ~enabled=true) => {
  let newStyle = ReactDOM.Style.make(
    ~padding="0.2rem 0.3rem",
    ~margin="0.125rem 0.2rem",
    ~fontSize="small",
    (),
  )
  let style = switch givenStyle {
  | Some(givenStyle) => ReactDOM.Style.combine(newStyle, givenStyle)
  | None => newStyle
  }
  <input type_={"button"} value onClick style disabled={!enabled} />
}
