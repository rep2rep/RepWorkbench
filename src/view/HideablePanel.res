module BoolStore = LocalStorage.MakeJsonable(Bool)

@react.component
let make = (~id, ~style as givenStyle, ~toggle, ~ref_=?, ~className=?, ~children) => {
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

  let style = {
    if hidden {
      ReactDOM.Style.combine(givenStyle, ReactDOM.Style.make(~display="none", ()))
    } else {
      givenStyle
    }
  }
  let ref = ref_

  <>
    {React.cloneElement(toggle(~hidden), {"onClick": toggleHide})}
    <div id style ?className ?ref> children </div>
  </>
}
