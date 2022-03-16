module BoolStore = LocalStorage.MakeJsonable(Bool)

module WarningOrError = (
  T: {
    type t
    let id: t => Gid.t
    let node: t => Gid.t
    let message: t => string
    let details: t => string
  },
) => {
  @react.component
  let make = (~data, ~onClick as givenOnClick=?, ~selected=false) => {
    let (showDetails, setShowDetails) = React.useState(_ => false)

    let toggle = _ => setShowDetails(s => !s)

    let onClick = givenOnClick->Option.map((givenOnClick, e) => {
      ReactEvent.Mouse.stopPropagation(e)
      givenOnClick(e, data)
    })
    <div
      ?onClick
      style={ReactDOM.Style.make(
        ~padding="0.1rem 0.5rem",
        ~cursor="pointer",
        ~fontSize="0.95rem",
        ~background={
          if selected {
            "rgba(220,220,220,1)"
          } else {
            "rgba(0,0,0,0)"
          }
        },
        (),
      )}>
      <span
        onClick=toggle
        style={ReactDOM.Style.make(
          ~position="relative",
          ~top="-0.1em",
          ~display="inline-block",
          ~width="1rem",
          ~fontSize="0.6rem",
          ~color="rgba(150, 150, 150, 1)",
          (),
        )}>
        {if showDetails {
          String.fromCodePoint(9660)
        } else {
          String.fromCodePoint(9654)
        }->React.string}
      </span>
      {React.string(T.message(data))}
      {if showDetails {
        <div
          style={ReactDOM.Style.make(
            ~fontSize="0.9rem",
            ~borderLeft="1px solid rgba(150, 150, 150, 1)",
            ~padding="0 0 0 0.5rem",
            ~margin="0.25rem",
            (),
          )}>
          {React.string(T.details(data))}
        </div>
      } else {
        React.null
      }}
    </div>
  }
}

module Warning = WarningOrError(ModelWarning)
module Error = WarningOrError(ModelError)

module Indicator = {
  @react.component
  let make = (~status: [#ok | #warning | #error | #loading]) => {
    if status === #loading {
      <>
        <style>
          {React.string(`
@keyframes kf-loading-intelligence {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}
`)}
        </style>
        <span
          style={ReactDOM.Style.make(
            ~width="12px",
            ~height="12px",
            ~position="relative",
            ~borderRadius="6px",
            ~display="inline-block",
            ~margin="0 0.5em",
            ~padding="0",
            (),
          )}>
          <span
            style={ReactDOM.Style.make(
              ~display="inline-block",
              ~width="10px",
              ~height="10px",
              ~borderRadius="5px",
              ~border="1px solid #666",
              ~borderColor="#666 transparent #666  transparent",
              ~animation="kf-loading-intelligence 1.2s linear infinite",
              (),
            )}
          />
        </span>
      </>
    } else {
      <span
        style={ReactDOM.Style.make(
          ~width="12px",
          ~height="12px",
          ~position="relative",
          ~borderRadius="6px",
          ~border="1px solid #888",
          ~display="inline-block",
          ~margin="0 0.5em",
          ~backgroundColor={
            switch status {
            | #ok => "rgb(0, 222, 0)"
            | #warning => "rgb(255, 222, 0)"
            | #error => "rgb(255, 0, 0)"
            | #loading => "rgba(0,0,0,0)"
            }
          },
          (),
        )}
      />
    }
  }
}

@react.component
let make = (
  ~warnings,
  ~errors,
  ~onClickWarning=?,
  ~onClickError=?,
  ~onDeselect=?,
  ~isUpToDate,
  ~selected,
  ~style as givenStyle=?,
) => {
  let (visible, setVisible) = React.useState(_ =>
    BoolStore.get("ERROR_PANEL_VISIBLE")->Or_error.getWithDefault(false)
  )

  let toggle = _ => {
    BoolStore.set("ERROR_PANEL_VISIBLE", !visible)
    if visible {
      switch onDeselect {
      | None => ()
      | Some(f) => f()
      }
    }
    setVisible(s => !s)
  }

  let commonStyle = ReactDOM.Style.make(
    ~background="white",
    ~bottom="0",
    ~position="absolute",
    ~borderTop="1px solid black",
    ~display="flex",
    ~flexDirection="column",
    ~margin="0",
    ~padding="0.25em",
    ~width="100%",
    (),
  )
  let hiddenStyle = commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~height="32px", ()))
  let visibleStyle = commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~height="200px", ()))
  let defaultStyle = if visible {
    visibleStyle
  } else {
    hiddenStyle
  }
  let style =
    givenStyle
    ->Option.map(ReactDOM.Style.combine(defaultStyle))
    ->Option.getWithDefault(defaultStyle)

  let (nErrors, nWarnings) = (Array.length(errors), Array.length(warnings))

  <div
    style
    onClick={_ =>
      switch onDeselect {
      | None => ()
      | Some(f) => f()
      }}>
    <div style={ReactDOM.Style.make(~position="relative", ~top="2px", ())}>
      <Indicator
        status={if !isUpToDate {
          #loading
        } else {
          switch (nErrors, nWarnings) {
          | (0, 0) => #ok
          | (0, _) => #warning
          | (_, _) => #error
          }
        }}
      />
      {React.string(
        switch (nErrors, nWarnings) {
        | (0, 0) => "Ok"
        | (1, 1) => "1 error, 1 warning."
        | (1, _) => "1 error, " ++ Int.toString(nWarnings) ++ " warnings."
        | (_, 1) => Int.toString(nErrors) ++ " errors, 1 warning."
        | (_, _) => Int.toString(nErrors) ++ " errors, " ++ Int.toString(nWarnings) ++ " warnings."
        },
      )}
      {if nErrors + nWarnings > 0 {
        <>
          <span style={ReactDOM.Style.make(~display="inline-block", ~width="0.5em", ())} />
          <Button
            value={if visible {
              "Hide"
            } else {
              "Show"
            }}
            onClick=toggle
          />
        </>
      } else {
        React.null
      }}
    </div>
    {if visible {
      <div
        style={ReactDOM.Style.make(
          ~width="calc(100% - 1rem)",
          ~flexGrow="1",
          ~margin="0.25rem 0.5rem 0.5rem 0.5rem",
          ~padding="0.25rem 0",
          ~borderRadius="2px",
          ~border="1px solid #aaa",
          ~overflowY="scroll",
          (),
        )}>
        {errors
        ->Array.map(w => {
          let onClick = onClickError
          <Error
            data=w
            ?onClick
            key={Gid.toString(ModelError.id(w))}
            selected={selected
            ->Option.map(id => id === ModelError.id(w))
            ->Option.getWithDefault(false)}
          />
        })
        ->React.array}
        {if nErrors > 0 && nWarnings > 0 {
          <hr style={ReactDOM.Style.make(~margin="0.25rem 0", ())} />
        } else {
          React.null
        }}
        {warnings
        ->Array.map(w => {
          let onClick = onClickWarning
          <Warning
            data=w
            ?onClick
            key={Gid.toString(ModelWarning.id(w))}
            selected={selected
            ->Option.map(id => id === ModelWarning.id(w))
            ->Option.getWithDefault(false)}
          />
        })
        ->React.array}
      </div>
    } else {
      React.null
    }}
  </div>
}
