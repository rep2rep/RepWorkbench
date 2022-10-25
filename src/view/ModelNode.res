module Kind = {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token
    | Placeholder

  let r_const = Hash.unique()
  let s_const = Hash.unique()
  let d_const = Hash.unique()
  let t_const = Hash.unique()
  let p_const = Hash.unique()
  let hash = t =>
    switch t {
    | Representation => r_const
    | Scheme => s_const
    | Dimension => d_const
    | Token => t_const
    | Placeholder => p_const
    }

  module Stable = {
    module V1 = {
      type t =
        | Representation
        | Scheme
        | Dimension
        | Token

      let toJson = t =>
        switch t {
        | Representation => "Representation"
        | Scheme => "Scheme"
        | Dimension => "Dimension"
        | Token => "Token"
        }->String.toJson

      let fromJson = json =>
        json
        ->String.fromJson
        ->Or_error.flatMap(s =>
          switch s {
          | "Representation" => Or_error.create(Representation)
          | "Scheme" => Or_error.create(Scheme)
          | "Dimension" => Or_error.create(Dimension)
          | "Token" => Or_error.create(Token)
          | s => Or_error.error_ss(["Unknown Schema Kind '", s, "'"])
          }
        )
    }
    module V2 = {
      type t = t =
        | Representation
        | Scheme
        | Dimension
        | Token
        | Placeholder

      let toJson = t =>
        switch t {
        | Representation => "Representation"
        | Scheme => "Scheme"
        | Dimension => "Dimension"
        | Token => "Token"
        | Placeholder => "Placeholder"
        }->String.toJson

      let fromJson = json =>
        json
        ->String.fromJson
        ->Or_error.flatMap(s =>
          switch s {
          | "Representation" => Or_error.create(Representation)
          | "Scheme" => Or_error.create(Scheme)
          | "Dimension" => Or_error.create(Dimension)
          | "Token" => Or_error.create(Token)
          | "Placeholder" => Or_error.create(Placeholder)
          | s => Or_error.error_ss(["Unknown Schema Kind '", s, "'"])
          }
        )

      let v1_to_v2 = v1 =>
        switch v1 {
        | V1.Representation => Representation
        | V1.Scheme => Scheme
        | V1.Dimension => Dimension
        | V1.Token => Token
        }
    }
  }
}

module Payload = {
  type t = {
    kind: Kind.t,
    name: string,
    name_suffix: option<string>,
    reference: string,
    reference_suffix: option<string>,
    dashed: bool,
  }

  module Stable = {
    module V1 = {
      type t = {
        kind: Kind.Stable.V1.t,
        name: string,
        name_suffix: option<string>,
        reference: string,
        reference_suffix: option<string>,
        dashed: bool,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("kind", t.kind->Kind.Stable.V1.toJson),
          ("name", t.name->String.toJson),
          ("reference", t.reference->String.toJson),
          ("dashed", t.dashed->Bool.toJson),
          ("name_suffix", t.name_suffix->Option.toJson(String.toJson)),
          ("reference_suffix", t.reference_suffix->Option.toJson(String.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode node payload object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let kind = getValue("kind", Kind.Stable.V1.fromJson)
          let name = getValue("name", String.fromJson)
          let reference = getValue("reference", String.fromJson)
          let dashed = getValue("dashed", Bool.fromJson)
          let name_suffix = getValue("name_suffix", j => j->Option.fromJson(String.fromJson))
          let reference_suffix = getValue("reference_suffix", j =>
            j->Option.fromJson(String.fromJson)
          )
          Or_error.both6((
            kind,
            name,
            reference,
            dashed,
            name_suffix,
            reference_suffix,
          ))->Or_error.map(((kind, name, reference, dashed, name_suffix, reference_suffix)) => {
            kind: kind,
            name: name,
            reference: reference,
            dashed: dashed,
            name_suffix: name_suffix,
            reference_suffix: reference_suffix,
          })
        })
    }
    module V2 = {
      type t = t = {
        kind: Kind.Stable.V2.t,
        name: string,
        name_suffix: option<string>,
        reference: string,
        reference_suffix: option<string>,
        dashed: bool,
      }

      external v1_to_v2: V1.t => t = "%identity"

      let toJson = t =>
        Js.Dict.fromList(list{
          ("kind", t.kind->Kind.Stable.V2.toJson),
          ("name", t.name->String.toJson),
          ("reference", t.reference->String.toJson),
          ("dashed", t.dashed->Bool.toJson),
          ("name_suffix", t.name_suffix->Option.toJson(String.toJson)),
          ("reference_suffix", t.reference_suffix->Option.toJson(String.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode node payload object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let kind = getValue("kind", Kind.Stable.V2.fromJson)
          let name = getValue("name", String.fromJson)
          let reference = getValue("reference", String.fromJson)
          let dashed = getValue("dashed", Bool.fromJson)
          let name_suffix = getValue("name_suffix", j => j->Option.fromJson(String.fromJson))
          let reference_suffix = getValue("reference_suffix", j =>
            j->Option.fromJson(String.fromJson)
          )
          Or_error.both6((
            kind,
            name,
            reference,
            dashed,
            name_suffix,
            reference_suffix,
          ))->Or_error.map(((kind, name, reference, dashed, name_suffix, reference_suffix)) => {
            kind: kind,
            name: name,
            reference: reference,
            dashed: dashed,
            name_suffix: name_suffix,
            reference_suffix: reference_suffix,
          })
        })
    }
  }

  let hash: t => Hash.t = Hash.record6(
    ("kind", Kind.hash),
    ("name", String.hash),
    ("name_suffix", Option.hash(_, String.hash)),
    ("reference", String.hash),
    ("reference_suffix", Option.hash(_, String.hash)),
    ("dashed", Bool.hash),
  )

  let kind = t => t.kind
  let dashed = t => t.dashed

  let trim = (s, length) =>
    if String.length(s) > length {
      s->String.substring(~from=0, ~to_=length - 3) ++ "..."
    } else {
      s
    }

  let rec adjust_label_to_width = (body, suffix, len, emwidth) => {
    let curr = switch suffix {
    | None => trim(body, len)
    | Some(suffix) => String.concatMany("", [trim(body, len), ", ", suffix])
    }
    let currwidth = curr->String.approximateEmWidth
    if currwidth > emwidth {
      adjust_label_to_width(body, suffix, len - 1, emwidth)
    } else {
      curr
    }
  }

  let visible_name_and_suffix = (t, emwidth) =>
    adjust_label_to_width(t.name, t.name_suffix, String.length(t.name), emwidth)

  let full_name_and_suffix = t =>
    switch t.name_suffix {
    | None => t.name
    | Some(suffix) => String.concatMany("", [t.name, ", ", suffix])
    }

  let visible_reference_and_suffix = (t, emwidth) =>
    adjust_label_to_width(t.reference, t.reference_suffix, String.length(t.reference), emwidth)

  let full_reference_and_suffix = t =>
    switch t.reference_suffix {
    | None => t.reference
    | Some(suffix) => String.concatMany("", [t.reference, ", ", suffix])
    }
}

type t = ReactD3Graph.Node.t<Payload.t>

let data = t => [t]

let pxToEm = px => px /. 15.
let emToPx = em => em *. 15.

module SchemaShape = {
  let style = (~dashed=false, selected) =>
    if selected && dashed {
      ReactDOM.Style.make(
        ~fill="rgb(220, 220, 220)",
        ~stroke="black",
        ~strokeWidth="2",
        ~strokeDasharray="5 3",
        (),
      )
    } else if selected && !dashed {
      ReactDOM.Style.make(~fill="rgb(220, 220, 220)", ~stroke="black", ~strokeWidth="2", ())
    } else if !selected && dashed {
      ReactDOM.Style.make(
        ~fill="white",
        ~stroke="black",
        ~strokeWidth="1",
        ~strokeDasharray="5 3",
        (),
      )
    } else {
      ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())
    }

  module Representation = {
    @react.component
    let make = (~width: float, ~height: float, ~selected: bool, ~children: React.element) => {
      let radius = (height /. 2.0)->Float.toString

      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          rx={radius}
          ry={radius}
          style={style(selected)}
        />
        children
      </svg>
    }
  }

  module Scheme = {
    @react.component
    let make = (~width: float, ~height: float, ~selected: bool, ~children: React.element) => {
      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          style={style(selected)}
        />
        children
      </svg>
    }
  }

  module Dimension = {
    @react.component
    let make = (~width: float, ~height: float, ~selected: bool, ~children: React.element) => {
      let mkPoint = ((x, y)) => Array.joinWith([Float.toString(x), Float.toString(y)], ",")

      let topInset = 8.
      let points = [
        (topInset +. 1., 1.),
        (width +. 1. -. topInset, 1.),
        (width +. 1., height +. 1.),
        (1., height +. 1.),
      ]

      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <polygon points={points->Array.map(mkPoint)->Array.joinWith(" ")} style={style(selected)} />
        children
      </svg>
    }
  }

  module Token = {
    @react.component
    let make = (
      ~width: float,
      ~height: float,
      ~selected: bool,
      ~dashed: bool,
      ~children: React.element,
    ) => {
      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          rx={"10"}
          ry={"10"}
          style={style(~dashed, selected)}
        />
        children
      </svg>
    }
  }

  module Placeholder = {
    module Hexagon = {
      @react.component
      let make = (~width: float, ~height: float, ~style) => {
        let inset = 14.
        let points =
          [
            (1., height /. 2.),
            (1. +. inset, 1.),
            (width -. 1. -. inset, 1.),
            (width -. 1., height /. 2.),
            (width -. 1. -. inset, height -. 1.),
            (1. +. inset, height -. 1.),
          ]
          ->Array.map(((x, y)) => Float.toString(x) ++ "," ++ Float.toString(y))
          ->Array.joinWith(" ")
        <polygon style points />
      }
    }

    module Octagon = {
      @react.component
      let make = (~width: float, ~height: float, ~style) => {
        let inset = 14.06
        let points =
          [
            (1., 1. +. inset),
            (1. +. inset, 1.),
            (width -. 1. -. inset, 1.),
            (width -. 1., 1. +. inset),
            (width -. 1., height -. inset -. 1.),
            (width -. 1. -. inset, height -. 1.),
            (1. +. inset, height -. 1.),
            (1., height -. inset -. 1.),
          ]
          ->Array.map(((x, y)) => Float.toString(x) ++ "," ++ Float.toString(y))
          ->Array.joinWith(" ")
        <polygon style points />
      }
    }

    @react.component
    let make = (
      ~width: float,
      ~height: float,
      ~isIntensional: bool,
      ~selected: bool,
      ~children: React.element,
    ) => {
      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        {if isIntensional {
          <Hexagon width height style={style(selected)} />
        } else {
          <Octagon width height style={style(selected)} />
        }}
        children
      </svg>
    }
  }
}

module SchemaText = {
  module EditableText = {
    type state = {
      value: string,
      editing: bool,
      changeSig: (Gid.t, string, string),
    }
    type event =
      | StartEdit(string)
      | MakeEdit(string)
      | FinishEdit

    let reducer = (state, event) => {
      switch event {
      | StartEdit(value) => {...state, value: value, editing: true}
      | MakeEdit(value) => {...state, value: value}
      | FinishEdit => {...state, editing: false}
      }
    }

    @react.component
    let make = (
      ~showText: string,
      ~hoverText: string,
      ~editText: string,
      ~width: float,
      ~height: float,
      ~y,
      ~changeSig: (Gid.t, string, string),
      ~onTab=?,
      ~startEditing=?,
    ) => {
      React.useEffect0(() => {
        Some(() => ModelNodeEdit.clearLocal())
      })
      let (state, dispatch) = React.useReducer(
        reducer,
        {value: editText, editing: false, changeSig: changeSig},
      )
      if startEditing->Option.getWithDefault(false) && !state.editing {
        ModelNodeEdit.callLocal(ModelSelection.empty)
        dispatch(StartEdit(editText))
      }
      if state.editing {
        let (id, _, _) = changeSig
        ModelNodeEdit.setLocal(selection =>
          if selection->ModelSelection.nodes->Array.includes(id)->not && state.editing {
            ModelNodeEdit.clearLocal()
            dispatch(FinishEdit)
          }
        )
        <foreignObject width={Float.toString(width)} height={Float.toString(height)}>
          <input
            autoFocus={true}
            onKeyUp={e => {
              ReactEvent.Keyboard.stopPropagation(e)
              let key = ReactEvent.Keyboard.key(e)
              if key === "Enter" || key === "Escape" || key === "Tab" {
                ModelNodeEdit.clearLocal()
                dispatch(FinishEdit)
                let (id, schema, slot) = state.changeSig
                ModelNodeEdit.callGlobal(id, schema, slot, state.value)
                if key === "Tab" {
                  onTab->Option.iter(f => f(e))
                }
              }
            }}
            onChange={e => {
              let newValue = ReactEvent.Form.target(e)["value"]
              dispatch(MakeEdit(newValue))
              let (id, schema, slot) = state.changeSig
              ModelNodeEdit.callGlobal(id, schema, slot, newValue)
            }}
            onBlur={e => {
              ReactEvent.Focus.target(e)["focus"](.)
            }}
            type_="text"
            value={state.value}
            style={ReactDOM.Style.make(
              ~width={Float.toString(width -. 20.) ++ "px"},
              ~marginTop={Float.toString(Float.fromString(y)->Option.getExn -. 15.) ++ "px"},
              ~marginLeft="11px",
              ~fontSize="0.84rem",
              ~textAlign="center",
              ~fontFamily="sans-serif",
              (),
            )}
          />
        </foreignObject>
      } else {
        <text
          x="50%"
          y
          textAnchor={"middle"}
          onDoubleClick={_ => {
            ModelNodeEdit.callLocal(ModelSelection.empty)
            dispatch(StartEdit(editText))
          }}>
          <title> {React.string(hoverText)} </title> {React.string(showText)}
        </text>
      }
    }
  }

  module OneLine = {
    @react.component
    let make = (
      ~payload: Payload.t,
      ~width: float,
      ~height: float,
      ~changeSig: (Gid.t, string, string),
    ) => {
      let showText = payload->Payload.visible_name_and_suffix(width->pxToEm)
      let hoverText = payload->Payload.full_name_and_suffix

      <g>
        <EditableText
          showText
          editText=payload.name
          hoverText
          width
          height
          changeSig
          y={Float.toString(height *. 0.57) ++ "px"}
        />
      </g>
    }
  }

  @react.component
  let make = (
    ~payload: Payload.t,
    ~width: float,
    ~height: float,
    ~changeSigTop: (Gid.t, string, string),
    ~changeSigBottom: (Gid.t, string, string),
  ) => {
    let (startEditing, setStartEditing) = React.useState(_ => 0)
    if startEditing == 1 || startEditing == 2 {
      Js.Global.setTimeout(() => setStartEditing(_ => 0), 50)->ignore
    }
    let topText = payload->Payload.visible_name_and_suffix(width->pxToEm)
    let bottomText = payload->Payload.visible_reference_and_suffix(width->pxToEm)
    let hoverTopText = payload->Payload.full_name_and_suffix
    let hoverBottomText = payload->Payload.full_reference_and_suffix
    <g>
      <line
        x1="8"
        y1="50%"
        x2={Float.toString(width -. 8.)}
        y2="50%"
        style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
      />
      <EditableText
        y={Float.toString(height /. 2. -. 5.)}
        showText=topText
        hoverText=hoverTopText
        editText=payload.name
        width
        height
        changeSig=changeSigTop
        onTab={e =>
          if !ReactEvent.Keyboard.shiftKey(e) {
            setStartEditing(_ => 2)
          } else {
            setStartEditing(_ => 0)
          }}
        startEditing={startEditing == 1}
      />
      <EditableText
        y={Float.toString(height -. 8.)}
        showText=bottomText
        hoverText=hoverBottomText
        editText=payload.reference
        width
        height
        changeSig=changeSigBottom
        onTab={e =>
          if ReactEvent.Keyboard.shiftKey(e) {
            setStartEditing(_ => 1)
          } else {
            setStartEditing(_ => 0)
          }}
        startEditing={startEditing == 2}
      />
    </g>
  }
}

module Configs = {
  let size = (width, height) =>
    {
      "width": 10.0 *. width +. 50.0,
      "height": 10.0 *. height +. 20.0,
    }

  let representation = (id, width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Representation
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            payload
            width={width}
            height={height}
            changeSigTop={(id, "Representation", "Domain")}
            changeSigBottom={(id, "Representation", "Display")}
          />
        </SchemaShape.Representation>
      },
      (),
    )
  }

  let scheme = (id, width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Scheme
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            payload
            width={width}
            height={height}
            changeSigTop={(id, "Scheme", "Concept")}
            changeSigBottom={(id, "Scheme", "Graphic")}
          />
        </SchemaShape.Scheme>
      },
      (),
    )
  }

  let dimension = (id, width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Dimension
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            payload
            width={width}
            height={height}
            changeSigTop={(id, "Dimension", "Concept")}
            changeSigBottom={(id, "Dimension", "Graphic")}
          />
        </SchemaShape.Dimension>
      },
      (),
    )
  }

  let token = (id, width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Token
          width={width}
          height={height}
          selected={ReactD3Graph.Node.selected(node)}
          dashed={payload->Payload.dashed}>
          <SchemaText
            payload
            width={width}
            height={height}
            changeSigTop={(id, "Token", "Concept")}
            changeSigBottom={(id, "Token", "Graphic")}
          />
        </SchemaShape.Token>
      },
      (),
    )
  }

  let placeholder = (id, width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        // We're blatantly abusing the "dashed" attribute to determine the shape!
        <SchemaShape.Placeholder
          width={width}
          height={height}
          isIntensional={payload->Payload.dashed}
          selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText.OneLine
            payload width={width} height={height} changeSig={(id, "Placeholder", "Description")}
          />
        </SchemaShape.Placeholder>
      },
      (),
    )
  }

  let create = (id, payload) => {
    let height = 50.
    let maxwidth = 500.
    let minwidth = 100.
    let width: float = Float.max(
      minwidth,
      Float.min(
        maxwidth,
        20. +.
        // Fudge factor!
        Float.max(
          Payload.visible_name_and_suffix(payload, pxToEm(maxwidth))->String.approximateEmWidth,
          Payload.visible_reference_and_suffix(
            payload,
            pxToEm(maxwidth),
          )->String.approximateEmWidth,
        )->emToPx,
      ),
    )
    switch Payload.kind(payload) {
    | Kind.Representation => representation(id, width, height)
    | Kind.Scheme => scheme(id, width, height)
    | Kind.Dimension => dimension(id, width, height)
    | Kind.Token => token(id, width, height)
    | Kind.Placeholder => placeholder(id, width, height)
    }
  }
}

let createSchema = (x, y, payload, config, id) => {
  let id = id->Gid.toString->ReactD3Graph.Node.Id.ofString
  ReactD3Graph.Node.create(~id, ~payload, ~config, ~x, ~y, ())
}

let create = (~name, ~reference, ~x, ~y, kind, id) => {
  let payload = {
    Payload.kind: kind,
    name: name,
    reference: reference,
    dashed: false,
    name_suffix: if kind == Kind.Dimension {
      Some("-")
    } else {
      None
    },
    reference_suffix: if kind == Kind.Dimension {
      Some("-")
    } else {
      None
    },
  }
  let config = Configs.create(id, payload)
  createSchema(x, y, payload, config, id)
}

let id = t => t->ReactD3Graph.Node.id->ReactD3Graph.Node.Id.toString->Gid.fromString
let payload = t => t->ReactD3Graph.Node.payload->Option.getExn
let kind = t => t->payload->Payload.kind
let position = t => (t->ReactD3Graph.Node.x, t->ReactD3Graph.Node.y)
let setPosition = (t, ~x, ~y) => t->ReactD3Graph.Node.setX(x)->ReactD3Graph.Node.setY(y)
let updateConfig = (t, f) => t->ReactD3Graph.Node.updateConfig(f)
let updatePayload = (t, f) => {
  let t' = t->ReactD3Graph.Node.updatePayload(n => n->Option.map(f))
  t'->updateConfig(_ => Configs.create(t'->id, t'->ReactD3Graph.Node.payload->Option.getExn))
}

let hash = t => {
  let (x, y) = position(t)
  let payload = t->ReactD3Graph.Node.payload
  Hash.combine([Gid.hash(id(t)), Float.hash(x), Float.hash(y), Option.hash(payload, Payload.hash)])
}

// TODO: Check something!
let isValid = _ => Result.Ok()

let dupWithNewId = (t, id) =>
  ReactD3Graph.Node.setId(t, id->Gid.toString->ReactD3Graph.Node.Id.ofString)->updatePayload(p => p)

module Stable = {
  module V1 = {
    type t = ReactD3Graph.Node.t<Payload.Stable.V1.t>

    let toJson = t =>
      Js.Dict.fromList(list{
        ("payload", t->ReactD3Graph.Node.payload->Option.getExn->Payload.Stable.V1.toJson),
        ("x", t->ReactD3Graph.Node.x->Float.toJson),
        ("y", t->ReactD3Graph.Node.y->Float.toJson),
        ("id", t->ReactD3Graph.Node.id->ReactD3Graph.Node.Id.toString->String.toJson),
      })->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode ModelNode object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let payload = getValue("payload", Payload.Stable.V1.fromJson)
        let x = getValue("x", Float.fromJson)
        let y = getValue("y", Float.fromJson)
        let id = getValue("id", json =>
          json->String.fromJson->Or_error.map(ReactD3Graph.Node.Id.ofString)
        )

        Or_error.both4((payload, x, y, id))->Or_error.map(((payload, x, y, id)) => {
          let config =
            Configs.create(
              id->ReactD3Graph.Node.Id.toString->Gid.fromString,
              payload->Payload.Stable.V2.v1_to_v2,
            )->Obj.magic // DANGEROUS!!!
          ReactD3Graph.Node.create(~id, ~payload, ~config, ~x, ~y, ())
        })
      })
  }
  module V2 = {
    type t = ReactD3Graph.Node.t<Payload.Stable.V2.t>

    let v1_to_v2 = v1 => {
      v1->ReactD3Graph.Node.updatePayload(Option.map(_, Payload.Stable.V2.v1_to_v2))
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(2)),
        ("payload", t->ReactD3Graph.Node.payload->Option.getExn->Payload.Stable.V2.toJson),
        ("x", t->ReactD3Graph.Node.x->Float.toJson),
        ("y", t->ReactD3Graph.Node.y->Float.toJson),
        ("id", t->ReactD3Graph.Node.id->ReactD3Graph.Node.Id.toString->String.toJson),
      })->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode ModelNode object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let version = getValue("version", Int.fromJson)
        if Or_error.isOk(version) {
          let v = Or_error.okExn(version)
          if v === 2 {
            let payload = getValue("payload", Payload.Stable.V2.fromJson)
            let x = getValue("x", Float.fromJson)
            let y = getValue("y", Float.fromJson)
            let id = getValue("id", json =>
              json->String.fromJson->Or_error.map(ReactD3Graph.Node.Id.ofString)
            )

            Or_error.both4((payload, x, y, id))->Or_error.map(((payload, x, y, id)) => {
              let config = Configs.create(
                id->ReactD3Graph.Node.Id.toString->Gid.fromString,
                payload,
              )
              ReactD3Graph.Node.create(~id, ~payload, ~config, ~x, ~y, ())
            })
          } else {
            Or_error.error_ss(["Unknown ModelNode version ", Int.toString(v)])
          }
        } else {
          V1.fromJson(json)->Or_error.map(v1_to_v2)
        }
      })
  }
}
