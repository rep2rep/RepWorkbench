module Representation = {
  type t = {
    domain: string,
    display: string,
    notes: string,
  }

  module Stable = {
    module V1 = {
      type t = t = {
        domain: string,
        display: string,
        notes: string,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("domain", String.toJson(t.domain)),
          ("display", String.toJson(t.display)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Schema slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let domain = getValue("domain", String.fromJson)
          let display = getValue("display", String.fromJson)
          let notes = getValue("notes", String.fromJson)

          Or_error.both3((domain, display, notes))->Or_error.map(((domain, display, notes)) => {
            domain: domain,
            display: display,
            notes: notes,
          })
        })
    }
  }

  let empty = {
    domain: "#Rep#",
    display: "#Ref#",
    notes: "",
  }
}

module Scheme = {
  type t = {
    concept_structure: string,
    graphic_structure: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    organisation: string,
    notes: string,
  }

  module Stable = {
    module V1 = {
      type t = t = {
        concept_structure: string,
        graphic_structure: string,
        function: Function.t,
        explicit: bool,
        scope: Scope.t,
        organisation: string,
        notes: string,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("concept_structure", String.toJson(t.concept_structure)),
          ("graphic_structure", String.toJson(t.graphic_structure)),
          ("function", Function.toJson(t.function)),
          ("explicit", Bool.toJson(t.explicit)),
          ("scope", Scope.toJson(t.scope)),
          ("organisation", String.toJson(t.organisation)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Schema slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let concept_structure = getValue("concept_structure", String.fromJson)
          let graphic_structure = getValue("graphic_structure", String.fromJson)
          let function = getValue("function", Function.fromJson)
          let explicit = getValue("explicit", Bool.fromJson)
          let scope = getValue("scope", Scope.fromJson)
          let organisation = getValue("organisation", String.fromJson)
          let notes = getValue("notes", String.fromJson)

          Or_error.both7((
            concept_structure,
            graphic_structure,
            function,
            explicit,
            scope,
            organisation,
            notes,
          ))->Or_error.map(((
            concept_structure,
            graphic_structure,
            function,
            explicit,
            scope,
            organisation,
            notes,
          )) => {
            concept_structure: concept_structure,
            graphic_structure: graphic_structure,
            function: function,
            explicit: explicit,
            scope: scope,
            organisation: organisation,
            notes: notes,
          })
        })
    }
  }

  let empty = {
    concept_structure: "#Sch#",
    graphic_structure: "#Ref#",
    function: Function.Semantic,
    explicit: true,
    scope: Scope.Global,
    organisation: "",
    notes: "",
  }
}

module Dimension = {
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

  module Stable = {
    module V1 = {
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

      let toJson = t =>
        Js.Dict.fromList(list{
          ("concept", String.toJson(t.concept)),
          ("concept_scale", Quantity_scale.toJson(t.concept_scale)),
          ("concept_attributes", t.concept_attributes->List.toJson(Concept_attribute.toJson)),
          ("graphic", String.toJson(t.graphic)),
          ("graphic_scale", Quantity_scale.toJson(t.graphic_scale)),
          ("graphic_attributes", t.graphic_attributes->List.toJson(Graphic_attribute.toJson)),
          ("function", Function.toJson(t.function)),
          ("scope", Scope.toJson(t.scope)),
          ("explicit", Bool.toJson(t.explicit)),
          ("organisation", String.toJson(t.organisation)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Schema slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let concept = getValue("concept", String.fromJson)
          let concept_scale = getValue("concept_scale", Quantity_scale.fromJson)
          let concept_attributes = getValue("concept_attributes", j =>
            j->List.fromJson(Concept_attribute.fromJson)
          )
          let graphic = getValue("graphic", String.fromJson)
          let graphic_scale = getValue("graphic_scale", Quantity_scale.fromJson)
          let graphic_attributes = getValue("graphic_attributes", j =>
            j->List.fromJson(Graphic_attribute.fromJson)
          )
          let function = getValue("function", Function.fromJson)
          let scope = getValue("scope", Scope.fromJson)
          let explicit = getValue("explicit", Bool.fromJson)
          let organisation = getValue("organisation", String.fromJson)
          let notes = getValue("notes", String.fromJson)

          Or_error.both11((
            concept,
            concept_scale,
            concept_attributes,
            graphic,
            graphic_scale,
            graphic_attributes,
            function,
            scope,
            explicit,
            organisation,
            notes,
          ))->Or_error.map(((
            concept,
            concept_scale,
            concept_attributes,
            graphic,
            graphic_scale,
            graphic_attributes,
            function,
            scope,
            explicit,
            organisation,
            notes,
          )) => {
            concept: concept,
            concept_scale: concept_scale,
            concept_attributes: concept_attributes,
            graphic: graphic,
            graphic_scale: graphic_scale,
            graphic_attributes: graphic_attributes,
            function: function,
            scope: scope,
            explicit: explicit,
            organisation: organisation,
            notes: notes,
          })
        })
    }
  }

  let empty = {
    concept: "#Dim#",
    concept_scale: Quantity_scale.Nominal,
    concept_attributes: list{},
    graphic: "#Ref#",
    graphic_scale: Quantity_scale.Nominal,
    graphic_attributes: list{},
    function: Function.Semantic,
    scope: Scope.Global,
    explicit: true,
    organisation: "",
    notes: "",
  }
}

module Token = {
  type t = {
    concept: string,
    graphic: string,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    notes: string,
  }

  module Stable = {
    module V1 = {
      type t = t = {
        concept: string,
        graphic: string,
        is_class: bool,
        function: Function.t,
        explicit: bool,
        notes: string,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("concept", String.toJson(t.concept)),
          ("graphic", String.toJson(t.graphic)),
          ("is_class", Bool.toJson(t.is_class)),
          ("function", Function.toJson(t.function)),
          ("explicit", Bool.toJson(t.explicit)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Schema slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let concept = getValue("concept", String.fromJson)
          let graphic = getValue("graphic", String.fromJson)
          let is_class = getValue("is_class", Bool.fromJson)
          let function = getValue("function", Function.fromJson)
          let explicit = getValue("explicit", Bool.fromJson)
          let notes = getValue("notes", String.fromJson)

          Or_error.both6((concept, graphic, is_class, function, explicit, notes))->Or_error.map(((
            concept,
            graphic,
            is_class,
            function,
            explicit,
            notes,
          )) => {
            concept: concept,
            graphic: graphic,
            is_class: is_class,
            function: function,
            explicit: explicit,
            notes: notes,
          })
        })
    }
  }

  let empty = {
    concept: "#Tok#",
    graphic: "#Ref#",
    is_class: false,
    function: Function.Semantic,
    explicit: true,
    notes: "",
  }
}

module Schema = {
  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)

  module Stable = {
    module V1 = {
      type t = t =
        | Representation(Representation.Stable.V1.t)
        | Scheme(Scheme.Stable.V1.t)
        | Dimension(Dimension.Stable.V1.t)
        | Token(Token.Stable.V1.t)

      let toJson = t => {
        let (kind, json) = switch t {
        | Representation(r) => ("representation", Representation.Stable.V1.toJson(r))
        | Scheme(s) => ("scheme", Scheme.Stable.V1.toJson(s))
        | Dimension(d) => ("dimension", Dimension.Stable.V1.toJson(d))
        | Token(t) => ("token", Token.Stable.V1.toJson(t))
        }
        Js.Dict.fromList(list{("kind", String.toJson(kind)), ("value", json)})->Js.Json.object_
      }

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Schema slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let kind = getValue("kind", String.fromJson)
          let value = getValue("value", j => Or_error.create(j))

          Or_error.both((kind, value))->Or_error.flatMap(((kind, value)) => {
            switch kind {
            | "representation" =>
              Representation.Stable.V1.fromJson(value)->Or_error.map(r => Representation(r))
            | "scheme" => Scheme.Stable.V1.fromJson(value)->Or_error.map(s => Scheme(s))
            | "dimension" => Dimension.Stable.V1.fromJson(value)->Or_error.map(d => Dimension(d))
            | "token" => Token.Stable.V1.fromJson(value)->Or_error.map(t => Token(t))
            | _ => Or_error.error_ss(["Unknown Schema slot kind '", kind, "'"])
            }
          })
        })
    }
  }

  let empty = kind =>
    switch kind {
    | ModelNode.Kind.Representation => Representation.empty->Representation
    | ModelNode.Kind.Scheme => Scheme.empty->Scheme
    | ModelNode.Kind.Dimension => Dimension.empty->Dimension
    | ModelNode.Kind.Token => Token.empty->Token
    }

  let name = t =>
    switch t {
    | Representation(r) => r.domain
    | Scheme(s) => s.concept_structure
    | Dimension(d) => d.concept
    | Token(t) => t.concept
    }

  let reference = t =>
    switch t {
    | Representation(r) => r.display
    | Scheme(s) => s.graphic_structure
    | Dimension(d) => d.graphic
    | Token(t) => t.graphic
    }
}

module Model = {
  type t = {name: string, notes: string}

  module Stable = {
    module V1 = {
      type t = t = {name: string, notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("name", String.toJson(t.name)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Model slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          version->Or_error.flatMap(version =>
            if version !== 1 {
              Or_error.error_ss([
                "Unable to decode Model Slots version ",
                Int.toString(version),
                ".",
              ])
            } else {
              let name = getValue("name", String.fromJson)
              let notes = getValue("notes", String.fromJson)
              Or_error.both((name, notes))->Or_error.map(((name, notes)) => {
                name: name,
                notes: notes,
              })
            }
          )
        })
    }
  }

  let name = t => t.name
  let notes = t => t.notes

  let create = (~name) => {name: name, notes: ""}
}

type t =
  | Empty
  | Global(Model.t)
  | Multiple(array<(Uuid.t, Schema.t)>)
  | Single(Uuid.t, Schema.t)
