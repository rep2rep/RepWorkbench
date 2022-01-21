module Kind = {
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

module Payload = {
  type t = {
    kind: Kind.t,
    name: string,
    reference: string,
  }

  let toJson = t =>
    Js.Dict.fromList(list{
      ("kind", t.kind->Kind.toJson),
      ("name", t.name->String.toJson),
      ("reference", t.reference->String.toJson),
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
      let kind = getValue("kind", Kind.fromJson)
      let name = getValue("name", String.fromJson)
      let reference = getValue("reference", String.fromJson)
      Or_error.both3((kind, name, reference))->Or_error.map(((kind, name, reference)) => {
        kind: kind,
        name: name,
        reference: reference,
      })
    })

  let create = (name, reference, kind) => {kind: kind, name: name, reference: reference}

  let name = t => t.name
  let reference = t => t.reference
}

type t = ReactD3Graph.Node.t<Payload.t>

let data = t => [t]

module SchemaShape = {
  let style = selected =>
    if selected {
      ReactDOM.Style.make(~fill="rgb(240, 240, 240)", ~stroke="black", ~strokeWidth="2", ())
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
    let make = (~width: float, ~height: float, ~selected: bool, ~children: React.element) => {
      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          rx={"10"}
          ry={"10"}
          style={style(selected)}
        />
        children
      </svg>
    }
  }
}

module SchemaText = {
  @react.component
  let make = (~topText: string, ~bottomText: string, ~width: float, ~height: float) =>
    <g>
      <line
        x1="8"
        y1="50%"
        x2={Float.toString(width -. 8.)}
        y2="50%"
        style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
      />
      <text x={"50%"} y={Float.toString(height /. 2. -. 5.)} textAnchor={"middle"}>
        {React.string(topText)}
      </text>
      <text x={"50%"} y={Float.toString(height -. 8.)} textAnchor={"middle"}>
        {React.string(bottomText)}
      </text>
    </g>
}

module Configs = {
  let size = (width, height) =>
    {
      "width": 10.0 *. width +. 20.0,
      "height": 10.0 *. height +. 20.0,
    }

  let representation = (width, height) => {
    ReactD3Graph.Node.Config.create(
      ~renderLabel=false,
      ~size=size(width, height),
      ~viewGenerator=node => {
        <SchemaShape.Representation
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.name}
            bottomText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.reference}
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
        <SchemaShape.Scheme
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.name}
            bottomText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.reference}
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
        <SchemaShape.Dimension
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.name}
            bottomText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.reference}
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
        <SchemaShape.Token
          width={width} height={height} selected={ReactD3Graph.Node.selected(node)}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.name}
            bottomText={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.reference}
            width={width}
            height={height}
          />
        </SchemaShape.Token>
      },
      (),
    )
  }

  let create = (kind, width, height) =>
    switch kind {
    | Kind.Representation => representation(width, height)
    | Kind.Scheme => scheme(width, height)
    | Kind.Dimension => dimension(width, height)
    | Kind.Token => token(width, height)
    }
}

let letter_scale_factor = 11
let width = (name, reference) =>
  (Int.max(String.length(name), String.length(reference)) * letter_scale_factor)->Int.toFloat
let height = 50.

let createSchema = (x, y, payload, config) => {
  let uuid = Uuid.create()->Uuid.toString
  ReactD3Graph.Node.create(~id=uuid->ReactD3Graph.Node.Id.ofString, ~payload, ~config, ~x, ~y, ())
}

let create = (~name, ~reference, ~x, ~y, kind) => {
  let payload = Payload.create(name, reference, kind)
  let width = width(name, reference)
  let height = height
  let config = Configs.create(kind, width, height)
  createSchema(x, y, payload, config)
}

let toJson = t => {
  Js.Console.log(t)
  Js.Dict.fromList(list{
    ("payload", t->ReactD3Graph.Node.payload->Option.getExn->Payload.toJson),
    ("x", t->ReactD3Graph.Node.x->Float.toJson),
    ("y", t->ReactD3Graph.Node.y->Float.toJson),
    ("id", t->ReactD3Graph.Node.id->ReactD3Graph.Node.Id.toString->String.toJson),
  })->Js.Json.object_
}

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
    let payload = getValue("payload", Payload.fromJson)
    let x = getValue("x", Float.fromJson)
    let y = getValue("y", Float.fromJson)
    let id = getValue("id", json =>
      json->String.fromJson->Or_error.map(ReactD3Graph.Node.Id.ofString)
    )

    Or_error.both4((payload, x, y, id))->Or_error.map(((payload, x, y, id)) => {
      let width = width(payload.name, payload.reference)
      let height = height
      let config = Configs.create(payload.kind, width, height)
      ReactD3Graph.Node.create(~id, ~payload, ~config, ~x, ~y, ())
    })
  })

let id = ReactD3Graph.Node.id
let setPosition = (t, ~x, ~y) => t->ReactD3Graph.Node.setX(x)->ReactD3Graph.Node.setY(y)
