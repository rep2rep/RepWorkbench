module Representation: {
  type t = {
    domain: string,
    display: string,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t = t
    }
  }

  let empty: t
}

module Scheme: {
  type t = {
    concept_structure: string,
    graphic_structure: string,
    function: option<Function.t>,
    explicit: option<bool>,
    scope: option<Scope.t>,
    organisation: string,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t
    }
    module V2: {
      type t = t
    }
  }

  let empty: t
}

module Dimension: {
  type t = {
    concept: string,
    concept_scale: option<Quantity_scale.t>,
    concept_attributes: list<string>,
    graphic: string,
    graphic_scale: option<Quantity_scale.t>,
    graphic_attributes: list<string>,
    function: option<Function.t>,
    scope: option<Scope.t>,
    explicit: option<bool>,
    organisation: string,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t
    }
    module V2: {
      type t = t
    }
  }

  let empty: t
}

module Token: {
  type t = {
    concept: string,
    graphic: string,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t = t
    }
  }

  let empty: t
}

module Placeholder: {
  type t = {
    description: string,
    isIntensional: bool,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t = t
    }
  }
}

module Schema: {
  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)
    | Placeholder(Placeholder.t)

  let empty: ModelNode.Kind.t => t
  let name: t => string
  let reference: t => string

  module Stable: {
    module V1: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }

    module V2: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
      let v1_to_v2: V1.t => t
    }
  }
}

module Model: {
  type t = {name: string, notes: string}

  module Stable: {
    module V1: {
      type t = t = {name: string, notes: string}
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }

  let name: t => string
  let notes: t => string

  let create: (~name: string) => t
}

type t =
  | Empty
  | Global(Model.t)
  | Multiple(array<(Uuid.t, Schema.t)>)
  | Single(Uuid.t, Schema.t)
