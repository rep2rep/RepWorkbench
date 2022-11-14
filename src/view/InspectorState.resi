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
    is_class: option<bool>,
    function: option<Function.t>,
    explicit: option<bool>,
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

module Placeholder: {
  type t = {
    description: string,
    isIntensional: option<bool>,
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

module Hierarchy: {
  type t = {order: option<int>, notes: string}

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
    }
  }
}
module Anchor: {
  type t = {order: option<int>, notes: string}

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
    }
  }
}

module Generic: {
  type t = {notes: string}

  module Stable: {
    module V1: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

module Link: {
  type t =
    | Hierarchy(Hierarchy.t)
    | Anchor(Anchor.t)
    | Generic(Generic.t)

  let empty: ModelLink.Kind.t => t

  module Stable: {
    module V1: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>

      let empty: ModelLink.Kind.Stable.V2.t => t
    }
    module V2: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

module SchemaOrLink: {
  type t =
    | Schema(Schema.t)
    | Link(Link.t)

  let duplicate: t => t
  let hash: t => Hash.t

  module Stable: {
    module V1: {
      type t =
        | Schema(Schema.Stable.V2.t)
        | Link(Link.Stable.V1.t)
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V2: {
      type t = t
      let v1_to_v2: V1.t => t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

module Model: {
  type t = {name: string, notes: string, metrics: ModelMetrics.t}

  module Stable: {
    module V1: {
      type t = {name: string, notes: string}
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V2: {
      type t = t
      let v1_to_v2: V1.t => t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }

  let hash: t => Hash.t

  let name: t => string
  let notes: t => string
  let metrics: t => ModelMetrics.t

  let create: (~name: string) => t
}

type t =
  | Empty
  | Global(Model.t)
  | Schema(Gid.t, Schema.t)
  | Link(Gid.t, Link.t)
  | Multiple(array<(Gid.t, SchemaOrLink.t)>)

let isValid: t => Result.t<unit, array<string>>
