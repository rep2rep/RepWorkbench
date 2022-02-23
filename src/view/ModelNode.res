module Kind = {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token

  module Stable = {
    module V1 = {
      type t = t =
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
      type t = t = {
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
  }

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
}

module SchemaText = {
  @react.component
  let make = (
    ~topText: string,
    ~bottomText: string,
    ~hoverTopText: string,
    ~hoverBottomText: string,
    ~width: float,
    ~height: float,
  ) => {
    <g>
      <line
        x1="8"
        y1="50%"
        x2={Float.toString(width -. 8.)}
        y2="50%"
        style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
      />
      <text x={"50%"} y={Float.toString(height /. 2. -. 5.)} textAnchor={"middle"}>
        <title> {React.string(hoverTopText)} </title> {React.string(topText)}
      </text>
      <text x={"50%"} y={Float.toString(height -. 8.)} textAnchor={"middle"}>
        <title> {React.string(hoverBottomText)} </title> {React.string(bottomText)}
      </text>
    </g>
  }
}

module Configs = {
  let size = (width, height) =>
    {
      "width": 10.0 *. width +. 50.0,
      "height": 10.0 *. height +. 20.0,
    }

  let representation = (width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Representation
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={payload->Payload.visible_name_and_suffix(width->pxToEm)}
            bottomText={payload->Payload.visible_reference_and_suffix(width->pxToEm)}
            hoverTopText={payload->Payload.full_name_and_suffix}
            hoverBottomText={payload->Payload.full_reference_and_suffix}
            width={width}
            height={height}
          />
        </SchemaShape.Representation>
      },
      (),
    )
  }

  let scheme = (width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Scheme
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={payload->Payload.visible_name_and_suffix(width->pxToEm)}
            bottomText={payload->Payload.visible_reference_and_suffix(width->pxToEm)}
            hoverTopText={payload->Payload.full_name_and_suffix}
            hoverBottomText={payload->Payload.full_reference_and_suffix}
            width={width}
            height={height}
          />
        </SchemaShape.Scheme>
      },
      (),
    )
  }

  let dimension = (width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Dimension
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={payload->Payload.visible_name_and_suffix(width->pxToEm)}
            bottomText={payload->Payload.visible_reference_and_suffix(width->pxToEm)}
            hoverTopText={payload->Payload.full_name_and_suffix}
            hoverBottomText={payload->Payload.full_reference_and_suffix}
            width={width}
            height={height}
          />
        </SchemaShape.Dimension>
      },
      (),
    )
  }

  let token = (width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        let payload = ReactD3Graph.Node.payload(node)->Option.getExn
        <SchemaShape.Token
          width={width}
          height={height}
          selected={ReactD3Graph.Node.selected(node)}
          dashed={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.dashed}>
          <SchemaText
            topText={payload->Payload.visible_name_and_suffix(width->pxToEm)}
            bottomText={payload->Payload.visible_reference_and_suffix(width->pxToEm)}
            hoverTopText={payload->Payload.full_name_and_suffix}
            hoverBottomText={payload->Payload.full_reference_and_suffix}
            width={width}
            height={height}
          />
        </SchemaShape.Token>
      },
      (),
    )
  }

  let create = payload => {
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
    | Kind.Representation => representation(width, height)
    | Kind.Scheme => scheme(width, height)
    | Kind.Dimension => dimension(width, height)
    | Kind.Token => token(width, height)
    }
  }
}

let createSchema = (x, y, payload, config, id) => {
  let id = id->Uuid.toString->ReactD3Graph.Node.Id.ofString
  ReactD3Graph.Node.create(~id, ~payload, ~config, ~x, ~y, ())
}

let create = (~name, ~reference, ~x, ~y, kind, id) => {
  let payload = {
    Payload.kind: kind,
    name: name,
    reference: reference,
    dashed: false,
    name_suffix: if kind == Kind.Dimension {
      Some("N")
    } else {
      None
    },
    reference_suffix: if kind == Kind.Dimension {
      Some("N")
    } else {
      None
    },
  }
  let config = Configs.create(payload)
  createSchema(x, y, payload, config, id)
}

let dupWithNewId = (t, id) =>
  ReactD3Graph.Node.setId(t, id->Uuid.toString->ReactD3Graph.Node.Id.ofString)

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
          let config = Configs.create(payload)
          ReactD3Graph.Node.create(~id, ~payload, ~config, ~x, ~y, ())
        })
      })
  }
}

let id = t => t->ReactD3Graph.Node.id->ReactD3Graph.Node.Id.toString->Uuid.fromString
let kind = t => t->ReactD3Graph.Node.payload->Option.getExn->Payload.kind
let position = t => (t->ReactD3Graph.Node.x, t->ReactD3Graph.Node.y)
let setPosition = (t, ~x, ~y) => t->ReactD3Graph.Node.setX(x)->ReactD3Graph.Node.setY(y)
let updateConfig = (t, f) => t->ReactD3Graph.Node.updateConfig(f)
let updatePayload = (t, f) => {
  let t' = t->ReactD3Graph.Node.updatePayload(n => n->Option.map(f))
  t'->updateConfig(_ => Configs.create(t'->ReactD3Graph.Node.payload->Option.getExn))
}
