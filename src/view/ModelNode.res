module Payload = {
  type t = {
    name: string,
    reference: string,
    selected: bool,
  }

  let create = (name, reference) => {name: name, reference: reference, selected: false}
  let dummy = {name: "", reference: "", selected: false}

  let name = t => t.name
  let reference = t => t.reference
  let selected = t => t.selected
}

type t = {focus: ReactD3Graph.Node.t<Payload.t>}

module Kind = {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token
}

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
          width={width}
          height={height}
          selected={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.selected}>
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
          width={width}
          height={height}
          selected={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.selected}>
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
          width={width}
          height={height}
          selected={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.selected}>
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
          width={width}
          height={height}
          selected={ReactD3Graph.Node.payload(node)->Option.getExn->Payload.selected}>
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
  let focus = ReactD3Graph.Node.create(
    ~id=uuid->ReactD3Graph.Node.Id.ofString,
    ~payload,
    ~config,
    ~x,
    ~y,
    (),
  )
  {focus: focus}
}

let create = (~name, ~reference, ~x, ~y, kind) => {
  let payload = Payload.create(name, reference)
  let width = width(name, reference)
  let height = height
  let config = Configs.create(kind, width, height)
  createSchema(x, y, payload, config)
}

let id = t => t.focus->ReactD3Graph.Node.id

let setSelected = (t, isSelected) => {
  ...t,
  focus: t.focus->ReactD3Graph.Node.updatePayload(payload =>
    payload->Option.map(payload => {...payload, selected: isSelected})
  ),
}
