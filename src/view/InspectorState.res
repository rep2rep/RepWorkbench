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
    function: option<Function.t>,
    explicit: option<bool>,
    scope: option<Scope.t>,
    organisation: string,
    notes: string,
  }

  module Stable = {
    module V1 = {
      type t = {
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

    module V2 = {
      type t = t = {
        concept_structure: string,
        graphic_structure: string,
        function: option<Function.t>,
        explicit: option<bool>,
        scope: option<Scope.t>,
        organisation: string,
        notes: string,
      }

      let v1_to_v2 = v1 => {
        concept_structure: v1.V1.concept_structure,
        graphic_structure: v1.V1.graphic_structure,
        function: Some(v1.V1.function),
        explicit: Some(v1.V1.explicit),
        scope: Some(v1.V1.scope),
        organisation: v1.V1.organisation,
        notes: v1.V1.notes,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(2)),
          ("concept_structure", String.toJson(t.concept_structure)),
          ("graphic_structure", String.toJson(t.graphic_structure)),
          ("function", t.function->Option.toJson(Function.toJson)),
          ("explicit", t.explicit->Option.toJson(Bool.toJson)),
          ("scope", t.scope->Option.toJson(Scope.toJson)),
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
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Err(_) => V1.fromJson(json)->Or_error.map(v1_to_v2)
          | Or_error.Ok(2) => {
              let concept_structure = getValue("concept_structure", String.fromJson)
              let graphic_structure = getValue("graphic_structure", String.fromJson)
              let function = getValue("function", Option.fromJson(_, Function.fromJson))
              let explicit = getValue("explicit", Option.fromJson(_, Bool.fromJson))
              let scope = getValue("scope", Option.fromJson(_, Scope.fromJson))
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
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss(["Unrecognised version of InspectorState.Scheme: ", Int.toString(v)])
          }
        })
    }
  }

  let empty = {
    concept_structure: "#Sch#",
    graphic_structure: "#Ref#",
    function: None,
    explicit: None,
    scope: None,
    organisation: "",
    notes: "",
  }
}

module Dimension = {
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

  module Stable = {
    module V1 = {
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

    module V2 = {
      type t = t = {
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

      let v1_to_v2 = v1 => {
        concept: v1.V1.concept,
        concept_scale: Some(v1.V1.concept_scale),
        concept_attributes: v1.V1.concept_attributes,
        graphic: v1.V1.graphic,
        graphic_scale: Some(v1.V1.graphic_scale),
        graphic_attributes: v1.V1.graphic_attributes,
        function: Some(v1.V1.function),
        scope: Some(v1.V1.scope),
        explicit: Some(v1.V1.explicit),
        organisation: v1.V1.organisation,
        notes: v1.V1.notes,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(2)),
          ("concept", String.toJson(t.concept)),
          ("concept_scale", t.concept_scale->Option.toJson(Quantity_scale.toJson)),
          ("concept_attributes", t.concept_attributes->List.toJson(String.toJson)),
          ("graphic", String.toJson(t.graphic)),
          ("graphic_scale", t.graphic_scale->Option.toJson(Quantity_scale.toJson)),
          ("graphic_attributes", t.graphic_attributes->List.toJson(String.toJson)),
          ("function", t.function->Option.toJson(Function.toJson)),
          ("scope", t.scope->Option.toJson(Scope.toJson)),
          ("explicit", t.explicit->Option.toJson(Bool.toJson)),
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

          let version = getValue("version", Int.fromJson)

          switch version->Or_error.match {
          | Err(_) => V1.fromJson(json)->Or_error.map(v1_to_v2)
          | Ok(2) => {
              let concept = getValue("concept", String.fromJson)
              let concept_scale = getValue(
                "concept_scale",
                Option.fromJson(_, Quantity_scale.fromJson),
              )
              let concept_attributes = getValue("concept_attributes", j =>
                j->List.fromJson(String.fromJson)
              )
              let graphic = getValue("graphic", String.fromJson)
              let graphic_scale = getValue(
                "graphic_scale",
                Option.fromJson(_, Quantity_scale.fromJson),
              )
              let graphic_attributes = getValue("graphic_attributes", j =>
                j->List.fromJson(String.fromJson)
              )
              let function = getValue("function", Option.fromJson(_, Function.fromJson))
              let scope = getValue("scope", Option.fromJson(_, Scope.fromJson))
              let explicit = getValue("explicit", Option.fromJson(_, Bool.fromJson))
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
            }
          | Ok(v) =>
            Or_error.error_ss(["Unknown version for InspectorState.Dimension: ", Int.toString(v)])
          }
        })
    }
  }

  let empty = {
    concept: "#Dim#",
    concept_scale: None,
    concept_attributes: list{},
    graphic: "#Ref#",
    graphic_scale: None,
    graphic_attributes: list{},
    function: None,
    scope: None,
    explicit: None,
    organisation: "",
    notes: "",
  }
}

module Token = {
  type t = {
    concept: string,
    graphic: string,
    is_class: option<bool>,
    function: option<Function.t>,
    explicit: option<bool>,
    notes: string,
  }

  module Stable = {
    module V1 = {
      type t = {
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

    module V2 = {
      type t = t = {
        concept: string,
        graphic: string,
        is_class: option<bool>,
        function: option<Function.t>,
        explicit: option<bool>,
        notes: string,
      }

      let v1_to_v2 = v1 => {
        concept: v1.V1.concept,
        graphic: v1.V1.graphic,
        is_class: Some(v1.V1.is_class),
        function: Some(v1.V1.function),
        explicit: Some(v1.V1.explicit),
        notes: v1.V1.notes,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(2)),
          ("concept", String.toJson(t.concept)),
          ("graphic", String.toJson(t.graphic)),
          ("is_class", t.is_class->Option.toJson(Bool.toJson)),
          ("function", t.function->Option.toJson(Function.toJson)),
          ("explicit", t.explicit->Option.toJson(Bool.toJson)),
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
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Err(_) => V1.fromJson(json)->Or_error.map(v1_to_v2)
          | Ok(2) => {
              let concept = getValue("concept", String.fromJson)
              let graphic = getValue("graphic", String.fromJson)
              let is_class = getValue("is_class", Option.fromJson(_, Bool.fromJson))
              let function = getValue("function", Option.fromJson(_, Function.fromJson))
              let explicit = getValue("explicit", Option.fromJson(_, Bool.fromJson))
              let notes = getValue("notes", String.fromJson)

              Or_error.both6((
                concept,
                graphic,
                is_class,
                function,
                explicit,
                notes,
              ))->Or_error.map(((concept, graphic, is_class, function, explicit, notes)) => {
                concept: concept,
                graphic: graphic,
                is_class: is_class,
                function: function,
                explicit: explicit,
                notes: notes,
              })
            }
          | Ok(v) =>
            Or_error.error_ss(["Unknown version for InspectorState.Token: ", Int.toString(v)])
          }
        })
    }
  }

  let empty = {
    concept: "#Sym#",
    graphic: "#Ref#",
    is_class: None,
    function: None,
    explicit: None,
    notes: "",
  }
}

module Placeholder = {
  type t = {
    description: string,
    isIntensional: option<bool>,
    notes: string,
  }

  module Stable = {
    module V1 = {
      type t = t = {
        description: string,
        isIntensional: option<bool>,
        notes: string,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("description", String.toJson(t.description)),
          ("isIntensional", t.isIntensional->Option.toJson(Bool.toJson)),
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
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Ok(1) => {
              let description = getValue("description", String.fromJson)
              let isIntensional = getValue("isIntensional", Option.fromJson(_, Bool.fromJson))
              let notes = getValue("notes", String.fromJson)

              Or_error.both3((description, isIntensional, notes))->Or_error.map(((
                description,
                isIntensional,
                notes,
              )) => {
                description: description,
                isIntensional: isIntensional,
                notes: notes,
              })
            }
          | Ok(v) =>
            Or_error.error_ss(["Unknown InspectorState.Placeholder version ", Int.toString(v)])
          | Err(_) => Or_error.error_s("Unable to determine InspectorState.Placeholder version")
          }
        })
    }
  }

  let empty = {
    description: "#Placeholder#",
    isIntensional: None,
    notes: "",
  }
}

module Schema = {
  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)
    | Placeholder(Placeholder.t)

  module Stable = {
    module V1 = {
      type t =
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

    module V2 = {
      type t = t =
        | Representation(Representation.Stable.V1.t)
        | Scheme(Scheme.Stable.V2.t)
        | Dimension(Dimension.Stable.V2.t)
        | Token(Token.Stable.V2.t)
        | Placeholder(Placeholder.Stable.V1.t)

      let v1_to_v2 = v1 =>
        switch v1 {
        | V1.Representation(r) => Representation(r)
        | V1.Scheme(s) => Scheme(s->Scheme.Stable.V2.v1_to_v2)
        | V1.Dimension(d) => Dimension(d->Dimension.Stable.V2.v1_to_v2)
        | V1.Token(t) => Token(t->Token.Stable.V2.v1_to_v2)
        }

      let toJson = t => {
        let (kind, json) = switch t {
        | Representation(r) => ("representation", Representation.Stable.V1.toJson(r))
        | Scheme(s) => ("scheme", Scheme.Stable.V2.toJson(s))
        | Dimension(d) => ("dimension", Dimension.Stable.V2.toJson(d))
        | Token(t) => ("token", Token.Stable.V2.toJson(t))
        | Placeholder(p) => ("placeholder", Placeholder.Stable.V1.toJson(p))
        }
        Js.Dict.fromList(list{
          ("version", Int.toJson(2)),
          ("kind", String.toJson(kind)),
          ("value", json),
        })->Js.Json.object_
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
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Err(_) => V1.fromJson(json)->Or_error.map(v1_to_v2)
          | Ok(2) => {
              let kind = getValue("kind", String.fromJson)
              let value = getValue("value", j => Or_error.create(j))

              Or_error.both((kind, value))->Or_error.flatMap(((kind, value)) => {
                switch kind {
                | "representation" =>
                  Representation.Stable.V1.fromJson(value)->Or_error.map(r => Representation(r))
                | "scheme" => Scheme.Stable.V2.fromJson(value)->Or_error.map(s => Scheme(s))
                | "dimension" =>
                  Dimension.Stable.V2.fromJson(value)->Or_error.map(d => Dimension(d))
                | "token" => Token.Stable.V2.fromJson(value)->Or_error.map(t => Token(t))
                | "placeholder" =>
                  Placeholder.Stable.V1.fromJson(value)->Or_error.map(t => Placeholder(t))
                | _ => Or_error.error_ss(["Unknown Schema slot kind '", kind, "'"])
                }
              })
            }
          | Ok(v) => Or_error.error_ss(["Unknown InspectorState version ", Int.toString(v)])
          }
        })
    }
  }

  let empty = kind =>
    switch kind {
    | ModelNode.Kind.Representation => Representation.empty->Representation
    | ModelNode.Kind.Scheme => Scheme.empty->Scheme
    | ModelNode.Kind.Dimension => Dimension.empty->Dimension
    | ModelNode.Kind.Token => Token.empty->Token
    | ModelNode.Kind.Placeholder => Placeholder.empty->Placeholder
    }

  let name = t =>
    switch t {
    | Representation(r) => r.domain
    | Scheme(s) => s.concept_structure
    | Dimension(d) => d.concept
    | Token(t) => t.concept
    | Placeholder(p) => p.description
    }

  let reference = t =>
    switch t {
    | Representation(r) => r.display
    | Scheme(s) => s.graphic_structure
    | Dimension(d) => d.graphic
    | Token(t) => t.graphic
    | Placeholder(_) => ""
    }
}

module Hierarchy = {
  type t = {notes: string}

  let empty = {notes: ""}

  module Stable = {
    module V1 = {
      type t = t = {notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Hierarchy slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let notes = getValue("notes", String.fromJson)
              notes->Or_error.map(notes => {notes: notes})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss([
              "Unrecognised version of InspectorState.Hierarchy: ",
              Int.toString(v),
            ])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}
module Anchor = {
  type t = {notes: string}

  let empty = {notes: ""}

  module Stable = {
    module V1 = {
      type t = t = {notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Anchor slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let notes = getValue("notes", String.fromJson)
              notes->Or_error.map(notes => {notes: notes})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss(["Unrecognised version of InspectorState.Anchor: ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module Relation = {
  type t = {notes: string}

  let empty = {notes: ""}

  module Stable = {
    module V1 = {
      type t = t = {notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Relation slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let notes = getValue("notes", String.fromJson)
              notes->Or_error.map(notes => {notes: notes})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss([
              "Unrecognised version of InspectorState.Relation: ",
              Int.toString(v),
            ])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module Overlap = {
  type t = {notes: string}

  let empty = {notes: ""}

  module Stable = {
    module V1 = {
      type t = t = {notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Overlap slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let notes = getValue("notes", String.fromJson)
              notes->Or_error.map(notes => {notes: notes})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss(["Unrecognised version of InspectorState.Overlap: ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}
module Disjoint = {
  type t = {notes: string}

  let empty = {notes: ""}

  module Stable = {
    module V1 = {
      type t = t = {notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Disjoint slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let notes = getValue("notes", String.fromJson)
              notes->Or_error.map(notes => {notes: notes})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss([
              "Unrecognised version of InspectorState.Disjoint: ",
              Int.toString(v),
            ])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module Generic = {
  type t = {notes: string}

  let empty = {notes: ""}

  module Stable = {
    module V1 = {
      type t = t = {notes: string}

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("notes", String.toJson(t.notes)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Generic slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let notes = getValue("notes", String.fromJson)
              notes->Or_error.map(notes => {notes: notes})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss(["Unrecognised version of InspectorState.Generic: ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module Link = {
  type t =
    | Hierarchy(Hierarchy.t)
    | Anchor(Anchor.t)
    | Relation(Relation.t)
    | Overlap(Overlap.t)
    | Disjoint(Disjoint.t)
    | Generic(Generic.t)

  let empty = kind =>
    switch kind {
    | ModelLink.Kind.Hierarchy => Hierarchy(Hierarchy.empty)
    | ModelLink.Kind.Anchor => Anchor(Anchor.empty)
    | ModelLink.Kind.Relation => Relation(Relation.empty)
    | ModelLink.Kind.Overlap => Overlap(Overlap.empty)
    | ModelLink.Kind.Disjoint => Disjoint(Disjoint.empty)
    | ModelLink.Kind.Generic => Generic(Generic.empty)
    }

  module Stable = {
    module V1 = {
      type t = t =
        | Hierarchy(Hierarchy.Stable.V1.t)
        | Anchor(Anchor.Stable.V1.t)
        | Relation(Relation.Stable.V1.t)
        | Overlap(Overlap.Stable.V1.t)
        | Disjoint(Disjoint.Stable.V1.t)
        | Generic(Generic.Stable.V1.t)

      let toJson = t => {
        let version = ("version", Int.toJson(1))
        switch t {
        | Hierarchy(rel) =>
          Js.Dict.fromList(list{
            version,
            ("kind", String.toJson("hierarchy")),
            ("payload", Hierarchy.Stable.V1.toJson(rel)),
          })
        | Anchor(rel) =>
          Js.Dict.fromList(list{
            version,
            ("kind", String.toJson("anchor")),
            ("payload", Anchor.Stable.V1.toJson(rel)),
          })

        | Relation(rel) =>
          Js.Dict.fromList(list{
            version,
            ("kind", String.toJson("relation")),
            ("payload", Relation.Stable.V1.toJson(rel)),
          })
        | Overlap(rel) =>
          Js.Dict.fromList(list{
            version,
            ("kind", String.toJson("overlap")),
            ("payload", Overlap.Stable.V1.toJson(rel)),
          })
        | Disjoint(rel) =>
          Js.Dict.fromList(list{
            version,
            ("kind", String.toJson("disjoint")),
            ("payload", Disjoint.Stable.V1.toJson(rel)),
          })
        | Generic(rel) =>
          Js.Dict.fromList(list{
            version,
            ("kind", String.toJson("generic")),
            ("payload", Generic.Stable.V1.toJson(rel)),
          })
        }->Js.Json.object_
      }

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Relation slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let kind = getValue("kind", String.fromJson)
              switch kind->Or_error.match {
              | Or_error.Ok("hierarchy") => {
                  let payload = getValue("payload", Hierarchy.Stable.V1.fromJson)
                  payload->Or_error.map(payload => Hierarchy(payload))
                }
              | Or_error.Ok("anchor") => {
                  let payload = getValue("payload", Anchor.Stable.V1.fromJson)
                  payload->Or_error.map(payload => Anchor(payload))
                }

              | Or_error.Ok("relation") => {
                  let payload = getValue("payload", Relation.Stable.V1.fromJson)
                  payload->Or_error.map(payload => Relation(payload))
                }
              | Or_error.Ok("overlap") => {
                  let payload = getValue("payload", Overlap.Stable.V1.fromJson)
                  payload->Or_error.map(payload => Overlap(payload))
                }
              | Or_error.Ok("disjoint") => {
                  let payload = getValue("payload", Disjoint.Stable.V1.fromJson)
                  payload->Or_error.map(payload => Disjoint(payload))
                }
              | Or_error.Ok("generic") => {
                  let payload = getValue("payload", Generic.Stable.V1.fromJson)
                  payload->Or_error.map(payload => Generic(payload))
                }
              | Or_error.Ok(k) => Or_error.error_ss(["Unrecognised InspectorState.Link kind: ", k])
              | Or_error.Err(e) => Or_error.error(e)
              }
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss(["Unrecognised version of InspectorState.Link: ", Int.toString(v)])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
  }
}

module SchemaOrLink = {
  type t =
    | Schema(Schema.t)
    | Link(Link.t)

  module Stable = {
    module V1 = {
      type t = t =
        | Schema(Schema.Stable.V2.t)
        | Link(Link.Stable.V1.t)

      let toJson = t => {
        let (kind, payload) = switch t {
        | Schema(s) => (String.toJson("schema"), Schema.Stable.V2.toJson(s))
        | Link(l) => (String.toJson("link"), Link.Stable.V1.toJson(l))
        }
        Js.Dict.fromList(list{
          ("version", Int.toJson(1)),
          ("kind", kind),
          ("payload", payload),
        })->Js.Json.object_
      }

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Relation slots object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Ok(1) => {
              let payload = getValue("payload", j => Or_error.create(j))
              let kind = getValue("kind", String.fromJson)
              (kind, payload)
              ->Or_error.both
              ->Or_error.flatMap(((kind, payload)) =>
                switch kind {
                | "schema" => Schema.Stable.V2.fromJson(payload)->Or_error.map(s => Schema(s))
                | "link" => Link.Stable.V1.fromJson(payload)->Or_error.map(l => Link(l))
                | _ =>
                  Or_error.error_ss(["Unrecognised kind of InspectorState.SchemaOrLink:", kind])
                }
              )
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss([
              "Unrecognised version of InspectorState.SchemaOrLink: ",
              Int.toString(v),
            ])
          | Or_error.Err(e) => Or_error.error(e)
          }
        })
    }
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
  | Schema(Gid.t, Schema.t)
  | Link(Gid.t, Link.t)
  | Multiple(array<(Gid.t, SchemaOrLink.t)>)
