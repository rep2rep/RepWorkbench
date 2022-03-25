module BoolStore = LocalStorage.MakeJsonable(Bool)
module GidSetStore = LocalStorage.MakeJsonable(Gid.Set)

module WarningOrError = (
  T: {
    type t
    let id: t => Gid.t
    let nodes: t => array<Gid.t>
    let message: t => string
    let details: t => string
    let suggestion: t => option<string>
  },
) => {
  @react.component
  let make = (
    ~data,
    ~onClick as givenOnClick=?,
    ~selected=false,
    ~kind: [#warning | #error],
    ~ignored=false,
    ~onIgnore=ignore,
  ) => {
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
        ~opacity={
          if ignored {
            "0.6"
          } else {
            "1"
          }
        },
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
            ~padding="0.25rem 0 0.25rem 0.5rem",
            ~margin="0.25rem",
            (),
          )}>
          {React.string(T.details(data))}
          {switch T.suggestion(data) {
          | None => React.null
          | Some(suggestion) =>
            <div style={ReactDOM.Style.make(~margin="0.25rem 0 0 0", ())}>
              <span style={ReactDOM.Style.make(~marginRight="0.5em", ~fontStyle="italic", ())}>
                {React.string("Suggestion:")}
              </span>
              {React.string(suggestion)}
            </div>
          }}
          {if kind === #warning {
            <div style={ReactDOM.Style.make(~marginTop="0.25rem", ())}>
              <label
                htmlFor={"warning-toggle-" ++ T.id(data)->Gid.toString}
                style={ReactDOM.Style.make(~fontSize="0.75rem", ~marginRight="0.5em", ())}>
                {React.string("Ignore")}
              </label>
              <input
                type_="checkbox"
                name={"warning-toggle-" ++ T.id(data)->Gid.toString}
                checked=ignored
                onChange={e => {
                  onIgnore(!ignored)
                }}
              />
            </div>
          } else {
            React.null
          }}
        </div>
      } else {
        React.null
      }}
    </div>
  }
}

module Warning = {
  module W = WarningOrError(ModelWarning)
  @react.component
  let make = (~data, ~onClick=?, ~selected=?, ~ignored=?, ~onIgnore=?) =>
    <W data ?onClick ?selected kind={#warning} ?ignored ?onIgnore />
}
module Error = {
  module E = WarningOrError(ModelError)
  @react.component
  let make = (~data, ~onClick=?, ~selected=?) => <E data ?onClick ?selected kind={#error} />
}

module Indicator = {
  let warning =
    <svg
      xmlns="http://www.w3.org/2000/svg"
      version="1.1"
      width="0.88em"
      height="0.8em"
      style={ReactDOM.Style.make(
        ~position="relative",
        ~top="0.05em",
        ~fill="none",
        ~stroke="currentColor",
        ~strokeLinejoin="round",
        ~strokeLinecap="round",
        (),
      )}
      viewBox="0 0 110 100">
      <path d="M 55,10 100,90 10,90 z" strokeWidth="10" />
      <path d="M 55,40 55,60" strokeWidth="12" />
      <path d="M 55,75 55,75" strokeWidth="12" />
    </svg>

  let error =
    <svg
      xmlns="http://www.w3.org/2000/svg"
      version="1.1"
      width="0.8em"
      height="0.8em"
      style={ReactDOM.Style.make(
        ~position="relative",
        ~top="0.05em",
        ~fill="none",
        ~stroke="currentColor",
        ~strokeLinejoin="round",
        ~strokeLinecap="round",
        (),
      )}
      viewBox="0 0 100 100">
      <path d="M 31,10 69,10 90,31 90,69 69,90 31,90 10,69 10,31 z" strokeWidth="10" />
      <path d="M 35,35 65,65" strokeWidth="12" />
      <path d="M 35,65 65,35" strokeWidth="12" />
    </svg>

  let spinner =
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

  let spacer = (~small) =>
    <span
      style={ReactDOM.Style.make(
        ~display="inline-block",
        ~width={
          if small {
            "0.3em"
          } else {
            "0.7em"
          }
        },
        (),
      )}
    />

  @react.component
  let make = (~status: [#ready | #loading], ~errors, ~warnings, ~ignoredWarnings=0) => {
    let errorString = React.string(
      switch errors {
      | 1 => "1 error"
      | n => Int.toString(n) ++ " errors"
      },
    )
    let warningString = React.string(
      switch warnings - ignoredWarnings {
      | 1 => "1 warning"
      | n => Int.toString(n) ++ " warnings"
      } ++
      switch ignoredWarnings {
      | 0 => ""
      | 1 => " (1 ignored warning)"
      | n => " (" ++ Int.toString(n) ++ " ignored warnings)"
      },
    )
    if status === #loading {
      <>
        {spinner}
        {spacer(~small=true)}
        {errorString}
        {spacer(~small=false)}
        {spinner}
        {spacer(~small=true)}
        {warningString}
      </>
    } else {
      <>
        {error}
        {spacer(~small=true)}
        {errorString}
        {spacer(~small=false)}
        {warning}
        {spacer(~small=true)}
        {warningString}
      </>
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
  let (ignoredWarnings, setIgnoredWarnings) = React.useState(_ =>
    GidSetStore.get("REPN_IGNORED_WARNINGS")->Or_error.getWithDefault(Gid.Set.empty)
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

  let ignoreWarning = id => {
    let ignored = ignoredWarnings->Gid.Set.add(id)
    GidSetStore.set("REPN_IGNORED_WARNINGS", ignored)
    setIgnoredWarnings(_ => ignored)
  }
  let showWarning = id => {
    let ignored = ignoredWarnings->Gid.Set.remove(id)
    GidSetStore.set("REPN_IGNORED_WARNINGS", ignored)
    setIgnoredWarnings(_ => ignored)
  }

  let (nErrors, nWarnings) = (Array.length(errors), Array.length(warnings))
  let nIgnoredWarnings =
    warnings
    ->Array.map(ModelWarning.id)
    ->Gid.Set.fromArray
    ->Gid.Set.intersect(ignoredWarnings)
    ->Gid.Set.size

  let visible = visible && nErrors + nWarnings > 0

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
  let hiddenStyle = commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~height="30px", ()))
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

  <div
    style
    onClick={_ =>
      switch onDeselect {
      | None => ()
      | Some(f) => f()
      }}>
    <div
      style={ReactDOM.Style.make(
        ~padding="0 0.5rem",
        ~fontSize="0.9rem",
        ~color={
          if visible {
            "black"
          } else {
            "#666"
          }
        },
        (),
      )}>
      <Indicator
        status={if !isUpToDate {
          #loading
        } else {
          #ready
        }}
        errors=nErrors
        warnings=nWarnings
        ignoredWarnings=nIgnoredWarnings
      />
      <span style={ReactDOM.Style.make(~display="inline-block", ~width="0.5em", ())} />
      <Button
        value={if visible {
          "Hide"
        } else {
          "Show"
        }}
        enabled={nErrors + nWarnings > 0}
        onClick=toggle
      />
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
            ignored={ignoredWarnings->Gid.Set.has(ModelWarning.id(w))}
            onIgnore={isIgnored =>
              if isIgnored {
                ignoreWarning(ModelWarning.id(w))
              } else {
                showWarning(ModelWarning.id(w))
              }}
          />
        })
        ->React.array}
      </div>
    } else {
      React.null
    }}
  </div>
}
