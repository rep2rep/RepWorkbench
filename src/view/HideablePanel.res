module BoolStore = LocalStorage.MakeJsonable(Bool)

@react.component
let make = (~id, ~style as givenStyle, ~className=?, ~side: [#left | #right], ~children) => {
  let (hidden, setHidden) = React.useState(_ => {
    BoolStore.get("HIDDEN-" ++ id)->Or_error.getWithDefault(false)
  })

  let toggleHide = _ => {
    if hidden {
      BoolStore.set("HIDDEN-" ++ id, false)
      setHidden(_ => false)
    } else {
      BoolStore.set("HIDDEN-" ++ id, true)
      setHidden(_ => true)
    }
  }

  let toggleStyle = {
    let common = ReactDOM.Style.make(~position="absolute", ~top="0", ~fontSize="28px", ())
    let sided = switch side {
    | #left => ReactDOM.Style.make(~left="5px", ())
    | #right => ReactDOM.Style.make(~right="5px", ())
    }
    ReactDOM.Style.combine(common, sided)
  }

  let style = {
    if hidden {
      ReactDOM.Style.combine(givenStyle, ReactDOM.Style.make(~display="none", ()))
    } else {
      givenStyle
    }
  }

  <>
    <div onClick={toggleHide} style=toggleStyle>
      {React.string(Js.String2.fromCharCode(8801))}
    </div>
    <div id style ?className> children </div>
  </>
}
