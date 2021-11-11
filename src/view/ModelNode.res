type t = ReactD3Graph.Node.t<ModelState.NodePayload.t>

module Kind = {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token
}

module SchemaShape = {
  module Representation = {
    @react.component
    let make = (~width: float, ~height: float, ~children: React.element) => {
      let radius = (height /. 2.0)->Float.toString

      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          rx={radius}
          ry={radius}
          style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
        />
        children
      </svg>
    }
  }

  module Scheme = {
    @react.component
    let make = (~width: float, ~height: float, ~children: React.element) => {
      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
        />
        children
      </svg>
    }
  }

  module Dimension = {
    @react.component
    let make = (~width: float, ~height: float, ~children: React.element) => {
      let mkPoint = ((x, y)) => Array.joinWith([Float.toString(x), Float.toString(y)], ",")

      let topInset = 8.
      let points = [
        (topInset +. 1., 1.),
        (width +. 1. -. topInset, 1.),
        (width +. 1., height +. 1.),
        (1., height +. 1.),
      ]

      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <polygon
          points={points->Array.map(mkPoint)->Array.joinWith(" ")}
          style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
        />
        children
      </svg>
    }
  }

  module Token = {
    @react.component
    let make = (~width: float, ~height: float, ~children: React.element) => {
      <svg width={Float.toString(width +. 2.)} height={Float.toString(height +. 2.)}>
        <rect
          x={"1"}
          y={"1"}
          width={Float.toString(width)}
          height={Float.toString(height)}
          rx={"10"}
          ry={"10"}
          style={ReactDOM.Style.make(~fill="white", ~stroke="black", ~strokeWidth="1", ())}
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
        <SchemaShape.Representation width={width} height={height}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.name)}
            bottomText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.reference)}
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
        <SchemaShape.Scheme width={width} height={height}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.name)}
            bottomText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.reference)}
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
        <SchemaShape.Dimension width={width} height={height}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.name)}
            bottomText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.reference)}
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
        <SchemaShape.Token width={width} height={height}>
          <SchemaText
            topText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.name)}
            bottomText={ReactD3Graph.Node.payload(node)
            ->Option.getExn
            ->(p => p.ModelState.NodePayload.reference)}
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
  Int.max(String.length(name), String.length(reference)) * letter_scale_factor
let height = 50

let createSchema = (x, y, payload, config) => {
  ReactD3Graph.Node.create(
    ~id=Uuid.create()->Uuid.toString->ReactD3Graph.Node.Id.ofString,
    ~payload,
    ~config,
    ~x,
    ~y,
    (),
  )
}

let create = (name, reference, x, y, kind) => {
  let payload = ModelState.NodePayload.create(name, reference)
  let width = width(name, reference)->Int.toFloat
  let height = height->Int.toFloat
  let config = Configs.create(kind, width, height)
  createSchema(x, y, payload, config)
}
