module Representation: {
  type t = {
    domain: string,
    display: string,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t = t = {
        domain: string,
        display: string,
        notes: string,
      }
    }
  }

  let empty: t
}

module Scheme: {
  type t = {
    concept_structure: string,
    graphic_structure: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    organisation: string,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t = t = {
        concept_structure: string,
        graphic_structure: string,
        function: Function.t,
        explicit: bool,
        scope: Scope.t,
        organisation: string,
        notes: string,
      }
    }
  }

  let empty: t
}

module Dimension: {
  type t = {
    concept: string,
    concept_scale: Quantity_scale.t,
    concept_attributes: list<Concept_attribute.t>,
    graphic: string,
    graphic_scale: Quantity_scale.t,
    graphic_attributes: list<Graphic_attribute.t>,
    function: Function.t,
    scope: Scope.t,
    explicit: bool,
    organisation: string,
    notes: string,
  }

  module Stable: {
    module V1: {
      type t = t = {
        concept: string,
        concept_scale: Quantity_scale.t,
        concept_attributes: list<Concept_attribute.t>,
        graphic: string,
        graphic_scale: Quantity_scale.t,
        graphic_attributes: list<Graphic_attribute.t>,
        function: Function.t,
        scope: Scope.t,
        explicit: bool,
        organisation: string,
        notes: string,
      }
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
      type t = t = {
        concept: string,
        graphic: string,
        is_class: bool,
        function: Function.t,
        explicit: bool,
        notes: string,
      }
    }
  }

  let empty: t
}

module Schema: {
  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)

  let applyEvent: (t, InspectorEvent.t) => t
  let empty: ModelNode.Kind.t => t
  let name: t => string
  let reference: t => string

  module Stable: {
    module V1: {
      type t = t =
        | Representation(Representation.Stable.V1.t)
        | Scheme(Scheme.Stable.V1.t)
        | Dimension(Dimension.Stable.V1.t)
        | Token(Token.Stable.V1.t)
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
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
  let applyEvent: (t, InspectorEvent.Model.t) => t
}

type t =
  | Empty
  | Global(Model.t)
  | Multiple(array<(Uuid.t, Schema.t)>)
  | Single(Uuid.t, Schema.t)
