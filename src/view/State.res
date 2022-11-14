let db = SetOnce.create()

module Model = {
  type t = {
    info: InspectorState.Model.t,
    graph: ModelState.t,
    slots: Gid.Map.t<InspectorState.SchemaOrLink.t>,
    intelligence: option<Intelligence_Intf.Response.t>,
    requestedIntelligence: option<Gid.t>,
    focusedIntelligence: option<Gid.t>,
  }

  module Stable = {
    module V1 = {
      type t = {
        id: Gid.t,
        name: string,
        model: ModelState.Stable.V1.t,
        slots: Gid.Map.t<InspectorState.Schema.Stable.V1.t>,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("id", t.id->Gid.toJson),
          ("name", t.name->String.toJson),
          ("model", t.model->ModelState.Stable.V1.toJson),
          ("slots", t.slots->Gid.Map.toJson(InspectorState.Schema.Stable.V1.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Model state object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let id = getValue("id", Gid.fromJson)
          let name = getValue("name", String.fromJson)
          let model = getValue("model", ModelState.Stable.V1.fromJson)
          let slots = getValue("slots", j =>
            j->Gid.Map.fromJson(InspectorState.Schema.Stable.V1.fromJson)
          )

          Or_error.both4((id, name, model, slots))->Or_error.map(((id, name, model, slots)) => {
            id: id,
            name: name,
            model: model,
            slots: slots,
          })
        })
    }

    module V2 = {
      type t = {
        info: InspectorState.Model.Stable.V1.t,
        graph: ModelState.Stable.V2.t,
        slots: Gid.Map.t<InspectorState.Schema.Stable.V1.t>,
      }

      let v1_to_v2 = t => {
        info: {InspectorState.Model.Stable.V1.name: t.V1.name, notes: ""},
        graph: t.V1.model->ModelState.Stable.V2.v1_to_v2,
        slots: t.V1.slots,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(2)),
          ("info", t.info->InspectorState.Model.Stable.V1.toJson),
          ("graph", t.graph->ModelState.Stable.V2.toJson),
          ("slots", t.slots->Gid.Map.toJson(InspectorState.Schema.Stable.V1.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Model state object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          if !Or_error.isOk(version) {
            Js.Console.log("Attempting to upgrade model from V1 to V2")
            V1.fromJson(json)->Or_error.map(v1_to_v2)
          } else if Or_error.okExn(version) != 2 {
            Or_error.error_ss([
              "Attempting to load unsupported Model version ",
              Int.toString(Or_error.okExn(version)),
              "!",
            ])
          } else {
            let info = getValue("info", InspectorState.Model.Stable.V1.fromJson)
            let graph = getValue("graph", ModelState.Stable.V2.fromJson)
            let slots = getValue("slots", j =>
              j->Gid.Map.fromJson(InspectorState.Schema.Stable.V1.fromJson)
            )

            Or_error.both3((info, graph, slots))->Or_error.map(((info, graph, slots)) => {
              info: info,
              graph: graph,
              slots: slots,
            })
          }
        })
    }

    module V3 = {
      type t = {
        info: InspectorState.Model.Stable.V1.t,
        graph: ModelState.Stable.V3.t,
        slots: Gid.Map.t<InspectorState.Schema.Stable.V2.t>,
      }

      let v2_to_v3 = t => {
        info: t.V2.info,
        graph: t.V2.graph->ModelState.Stable.V3.v2_to_v3,
        slots: t.V2.slots->Gid.Map.map(InspectorState.Schema.Stable.V2.v1_to_v2),
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(3)),
          ("info", t.info->InspectorState.Model.Stable.V1.toJson),
          ("graph", t.graph->ModelState.Stable.V3.toJson),
          ("slots", t.slots->Gid.Map.toJson(InspectorState.Schema.Stable.V2.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Model state object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          if !Or_error.isOk(version) {
            Js.Console.log("Attempting to upgrade model from V1 to V3")
            V1.fromJson(json)->Or_error.map(V2.v1_to_v2)->Or_error.map(v2_to_v3)
          } else if Or_error.okExn(version) == 2 {
            Js.Console.log("Attempting to upgrade model from V2 to V3")
            V2.fromJson(json)->Or_error.map(v2_to_v3)
          } else if Or_error.okExn(version) == 3 {
            let info = getValue("info", InspectorState.Model.Stable.V1.fromJson)
            let graph = getValue("graph", ModelState.Stable.V3.fromJson)
            let slots = getValue("slots", j =>
              j->Gid.Map.fromJson(InspectorState.Schema.Stable.V2.fromJson)
            )

            Or_error.both3((info, graph, slots))->Or_error.map(((info, graph, slots)) => {
              info: info,
              graph: graph,
              slots: slots,
            })
          } else {
            Or_error.error_ss([
              "Attempting to load unsupported Model version ",
              Int.toString(Or_error.okExn(version)),
              "!",
            ])
          }
        })
    }

    module V4 = {
      type t = {
        info: InspectorState.Model.Stable.V1.t,
        graph: ModelState.Stable.V4.t,
        slots: Gid.Map.t<InspectorState.SchemaOrLink.Stable.V1.t>,
        intelligence: option<Intelligence_Intf.Response.t>,
        requestedIntelligence: option<Gid.t>,
        focusedIntelligence: option<Gid.t>,
      }

      let v3_to_v4 = t => {
        let graph = t.V3.graph->ModelState.Stable.V4.v3_to_v4
        let slots = {
          let schemas =
            t.V3.slots->Gid.Map.map(s => InspectorState.SchemaOrLink.Stable.V1.Schema(s))
          let links =
            graph
            ->ModelState.Stable.V4.graph
            ->ModelGraph.Stable.V3.links
            ->Array.map(link => (
              ModelLink.Stable.V2.id(link),
              InspectorState.SchemaOrLink.Stable.V1.Link(
                InspectorState.Link.Stable.V1.empty(ModelLink.Stable.V2.kind(link)),
              ),
            ))
            ->Gid.Map.fromArray
          Gid.Map.merge(schemas, links, (_, l, r) =>
            switch (l, r) {
            | (Some(_), Some(_)) => None // Impossible?
            | (Some(l), None) => Some(l)
            | (None, Some(r)) => Some(r)
            | (None, None) => None // Impossible?
            }
          )
        }
        {
          info: t.V3.info,
          graph: graph,
          slots: slots,
          intelligence: None,
          requestedIntelligence: None,
          focusedIntelligence: None,
        }
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(4)),
          ("info", t.info->InspectorState.Model.Stable.V1.toJson),
          ("graph", t.graph->ModelState.Stable.V4.toJson),
          ("slots", t.slots->Gid.Map.toJson(InspectorState.SchemaOrLink.Stable.V1.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Model state object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Err(_) => {
              Js.Console.log("Attempting to upgrade model from V1 to V4")
              V1.fromJson(json)
              ->Or_error.map(V2.v1_to_v2)
              ->Or_error.map(V3.v2_to_v3)
              ->Or_error.map(v3_to_v4)
            }
          | Or_error.Ok(2) => {
              Js.Console.log("Attempting to upgrade model from V2 to V4")
              V2.fromJson(json)->Or_error.map(V3.v2_to_v3)->Or_error.map(v3_to_v4)
            }
          | Or_error.Ok(3) => {
              Js.Console.log("Attempting to upgrade model from V3 to V4")
              V3.fromJson(json)->Or_error.map(v3_to_v4)
            }
          | Or_error.Ok(4) => {
              let info = getValue("info", InspectorState.Model.Stable.V1.fromJson)
              let graph = getValue("graph", ModelState.Stable.V4.fromJson)
              let slots = getValue(
                "slots",
                Gid.Map.fromJson(_, InspectorState.SchemaOrLink.Stable.V1.fromJson),
              )

              (info, graph, slots)
              ->Or_error.both3
              ->Or_error.map(((info, graph, slots)) => {
                info: info,
                graph: graph,
                slots: slots,
                intelligence: None,
                requestedIntelligence: None,
                focusedIntelligence: None,
              })
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss([
              "Attempting to load unsupported Model version ",
              Int.toString(v),
              "!",
            ])
          }
        })
    }

    module V5 = {
      type t = t = {
        info: InspectorState.Model.Stable.V1.t,
        graph: ModelState.Stable.V5.t,
        slots: Gid.Map.t<InspectorState.SchemaOrLink.Stable.V2.t>,
        intelligence: option<Intelligence_Intf.Response.t>,
        requestedIntelligence: option<Gid.t>,
        focusedIntelligence: option<Gid.t>,
      }

      let v4_to_v5 = t => {
        let graph = t.V4.graph->ModelState.Stable.V5.v4_to_v5
        let slots = t.V4.slots->Gid.Map.map(InspectorState.SchemaOrLink.Stable.V2.v1_to_v2)
        {
          info: t.V4.info,
          graph: graph,
          slots: slots,
          intelligence: None,
          requestedIntelligence: None,
          focusedIntelligence: None,
        }
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("version", Int.toJson(5)),
          ("info", t.info->InspectorState.Model.Stable.V1.toJson),
          ("graph", t.graph->ModelState.Stable.V5.toJson),
          ("slots", t.slots->Gid.Map.toJson(InspectorState.SchemaOrLink.Stable.V2.toJson)),
        })->Js.Json.object_

      let fromJson = json =>
        json
        ->Js.Json.decodeObject
        ->Or_error.fromOption_s("Failed to decode Model state object JSON")
        ->Or_error.flatMap(dict => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Err(_) => {
              Js.Console.log("Attempting to upgrade model from V1 to V5")
              V1.fromJson(json)
              ->Or_error.map(V2.v1_to_v2)
              ->Or_error.map(V3.v2_to_v3)
              ->Or_error.map(V4.v3_to_v4)
              ->Or_error.map(v4_to_v5)
            }
          | Or_error.Ok(2) => {
              Js.Console.log("Attempting to upgrade model from V2 to V5")
              V2.fromJson(json)
              ->Or_error.map(V3.v2_to_v3)
              ->Or_error.map(V4.v3_to_v4)
              ->Or_error.map(v4_to_v5)
            }
          | Or_error.Ok(3) => {
              Js.Console.log("Attempting to upgrade model from V3 to V5")
              V3.fromJson(json)->Or_error.map(V4.v3_to_v4)->Or_error.map(v4_to_v5)
            }
          | Or_error.Ok(4) => {
              Js.Console.log("Attempting to upgrade model from V4 to V5")
              V4.fromJson(json)->Or_error.map(v4_to_v5)
            }
          | Or_error.Ok(5) => {
              let info = getValue("info", InspectorState.Model.Stable.V1.fromJson)
              let graph = getValue("graph", ModelState.Stable.V5.fromJson)
              let slots = getValue(
                "slots",
                Gid.Map.fromJson(_, InspectorState.SchemaOrLink.Stable.V2.fromJson),
              )

              (info, graph, slots)
              ->Or_error.both3
              ->Or_error.map(((info, graph, slots)) => {
                info: info,
                graph: graph,
                slots: slots,
                intelligence: None,
                requestedIntelligence: None,
                focusedIntelligence: None,
              })
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss([
              "Attempting to load unsupported Model version ",
              Int.toString(v),
              "!",
            ])
          }
        })
    }
  }

  module StorageMkr = (J: LocalStorage.Jsonable) => {
    module OldStorage = LocalStorage.MakeJsonable(J)
    let set = (key, value) => {
      let str = J.toJson(value)->Js.Json.stringify
      switch db->SetOnce.get {
      | None => Dialog.alert("Failed to save! Couldn't connect to database")
      | Some((db, store)) =>
        db
        ->IndexedDB.put(~store, ~key, str)
        ->Promise.catch(e => {
          Dialog.alert("Failed to save! Couldn't write data")
          Js.Console.log(e)
          Promise.resolve(str)
        })
        ->ignore
      }
    }

    let get = key => {
      switch db->SetOnce.get {
      | None =>
        Or_error.error_s("Failed to read model - could not connect to database!")->Promise.resolve
      | Some((db, store)) =>
        db
        ->IndexedDB.get(~store, ~key)
        ->Promise.thenResolve(s => s->Js.Json.parseExn->J.fromJson)
        ->Promise.catch(e => {
          Js.Console.log("Failed to load model " ++ key ++ ", checking LocalStorage.")
          Js.Console.log(e)
          let extant = OldStorage.get(key)
          if Or_error.isOk(extant) {
            Js.Console.log("Loaded " ++ key ++ " from LocalStorage. Removing old version.")
            OldStorage.delete(key)
          } else {
            Js.Console.log("Failed to find " ++ key ++ " in LocalStorage.")
          }
          Promise.resolve(extant)
        })
      }
    }

    let delete = key => {
      switch db->SetOnce.get {
      | None => Dialog.alert("Failed to delete model - could not connect to database!")
      | Some((db, store)) =>
        db
        ->IndexedDB.delete(~store, ~key)
        ->Promise.catch(e => {
          Js.Console.log3("Storage.Delete key", key, e)
          Dialog.alert("Failed to delete model!")
          Promise.resolve()
        })
        ->ignore
      }
    }
  }
  module Storage = StorageMkr(Stable.V5)

  let prefix = "RepNotation:Model:"
  let store = (t, id) => Storage.set(prefix ++ Gid.toString(id), t)
  let load = id => Storage.get(prefix ++ Gid.toString(id))
  let delete = id => Storage.delete(prefix ++ Gid.toString(id))

  let info = t => t.info
  let graph = t => t.graph
  let slots = t => t.slots

  let addToplevelNote = (t, note) => {...t, info: {...t.info, notes: t.info.notes ++ note}}

  let intelligence = t => t.intelligence
  let requestedIntelligence = t => t.requestedIntelligence
  let focusedIntelligence = t => t.focusedIntelligence

  let setIntelligence = (t, response) => {
    let seen_requested =
      (t.intelligence, t.requestedIntelligence)
      ->Option.both
      ->Option.map(((resp, req)) => resp.id === req)
      ->Option.getWithDefault(false)
    let matches_requested =
      (response, t.requestedIntelligence)
      ->Option.both
      ->Option.map(((resp, req)) => resp.Intelligence_Intf.Response.id === req)
      ->Option.getWithDefault(true)
    if seen_requested && !matches_requested {
      t
    } else {
      {...t, intelligence: response}
    }
  }
  let setRequestedIntelligence = (t, id) => {...t, requestedIntelligence: id}
  let setFocusedIntelligence = (t, id) => {...t, focusedIntelligence: id}

  let slotsForSelection = (t, selection) => {
    let ids = Array.concat(ModelSelection.nodes(selection), ModelSelection.links(selection))
    ids->Array.mapPartial(id => t.slots->Gid.Map.get(id)->Option.map(slots => (id, slots)))
  }

  let updateInfo = (t, info) => {...t, info: info}
  let updateGraph = (t, graph) => {...t, graph: graph}
  let updateSlots = (t, slots) => {...t, slots: slots}

  let create = name => {
    info: InspectorState.Model.create(~name),
    graph: ModelState.empty,
    slots: Gid.Map.empty(),
    intelligence: None,
    requestedIntelligence: None,
    focusedIntelligence: None,
  }

  let duplicate = (t, newName) => {
    let newIdMap = t.slots->Gid.Map.map(_ => Gid.create())
    {
      info: {...t.info, name: newName},
      graph: t.graph->ModelState.duplicate(newIdMap),
      slots: t.slots
      ->Gid.Map.toArray
      ->Array.map(((id, slots)) => (newIdMap->Gid.Map.get(id)->Option.getExn, slots))
      ->Gid.Map.fromArray,
      intelligence: None,
      requestedIntelligence: None,
      focusedIntelligence: None,
    }
  }

  let hash: t => Hash.t = Hash.record3(
    ("info", InspectorState.Model.hash),
    ("graph", ModelState.hash),
    (
      "slots",
      slots =>
        slots
        ->Gid.Map.toArray
        ->Array.hash(((id, slots)) =>
          Hash.combine([Gid.hash(id), InspectorState.SchemaOrLink.hash(slots)])
        ),
    ),
  )

  let isValid = t => {
    let checkSchemaMatches = (slots, node) =>
      switch (slots, node->ModelNode.kind) {
      | (InspectorState.Schema.Representation(slots), ModelNode.Kind.Representation) => {
          let payload = ModelNode.payload(node)
          let domainValid = if slots.domain === payload.name {
            Result.Ok()
          } else {
            Result.Error([
              "Representation schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched domain: " ++
              slots.domain ++
              " vs " ++
              payload.name,
            ])
          }
          let displayValid = if slots.display === payload.reference {
            Result.Ok()
          } else {
            Result.Error([
              "Representation schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched display: " ++
              slots.display ++
              " vs " ++
              payload.reference,
            ])
          }
          [domainValid, displayValid]->Result.allUnit(Array.concatMany)
        }
      | (InspectorState.Schema.Scheme(slots), ModelNode.Kind.Scheme) => {
          let payload = ModelNode.payload(node)
          let conceptValid = if slots.concept_structure === payload.name {
            Result.Ok()
          } else {
            Result.Error([
              "R-Scheme schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched concept structure: " ++
              slots.concept_structure ++
              " vs " ++
              payload.name,
            ])
          }
          let graphicValid = if slots.graphic_structure === payload.reference {
            Result.Ok()
          } else {
            Result.Error([
              "R-Scheme schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched graphic structure: " ++
              slots.graphic_structure ++
              " vs " ++
              payload.reference,
            ])
          }
          [conceptValid, graphicValid]->Result.allUnit(Array.concatMany)
        }
      | (InspectorState.Schema.Dimension(slots), ModelNode.Kind.Dimension) => {
          let payload = ModelNode.payload(node)
          let conceptValid = if slots.concept === payload.name {
            Result.Ok()
          } else {
            Result.Error([
              "R-Dimension schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched concept: " ++
              slots.concept ++
              " vs " ++
              payload.name,
            ])
          }
          let graphicValid = if slots.graphic === payload.reference {
            Result.Ok()
          } else {
            Result.Error([
              "R-Dimension schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched graphic: " ++
              slots.graphic ++
              " vs " ++
              payload.reference,
            ])
          }
          let conceptQSValid = if (
            slots.concept_scale
            ->Option.map(Quantity_scale.toString)
            ->Option.map(String.slice(_, ~from=0, ~to_=1))
            ->Option.getWithDefault("-")
            ->Some === payload.name_suffix
          ) {
            Result.Ok()
          } else {
            Result.Error([
              "R-Dimension schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched concept scale: " ++
              slots.concept_scale
              ->Option.map(Quantity_scale.toString)
              ->Option.map(String.slice(_, ~from=0, ~to_=1))
              ->Option.getWithDefault("-") ++
              " vs " ++
              payload.name_suffix->Option.getWithDefault("<unspecified>"),
            ])
          }
          let graphicQSValid = if (
            slots.graphic_scale
            ->Option.map(Quantity_scale.toString)
            ->Option.map(String.slice(_, ~from=0, ~to_=1))
            ->Option.getWithDefault("-")
            ->Some === payload.reference_suffix
          ) {
            Result.Ok()
          } else {
            Result.Error([
              "R-Dimension schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched graphic scale: " ++
              slots.graphic_scale
              ->Option.map(Quantity_scale.toString)
              ->Option.map(String.slice(_, ~from=0, ~to_=1))
              ->Option.getWithDefault("-") ++
              " vs " ++
              payload.reference_suffix->Option.getWithDefault("<unspecified>"),
            ])
          }
          [conceptValid, graphicValid, conceptQSValid, graphicQSValid]->Result.allUnit(
            Array.concatMany,
          )
        }
      | (InspectorState.Schema.Token(slots), ModelNode.Kind.Token) => {
          let payload = ModelNode.payload(node)
          let conceptValid = if slots.concept === payload.name {
            Result.Ok()
          } else {
            Result.Error([
              "R-Symbol schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched concept: " ++
              slots.concept ++
              " vs " ++
              payload.name,
            ])
          }
          let graphicValid = if slots.graphic === payload.reference {
            Result.Ok()
          } else {
            Result.Error([
              "R-Symbol schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched graphic: " ++
              slots.graphic ++
              " vs " ++
              payload.reference,
            ])
          }
          let dashedValid = if slots.is_class->Option.getWithDefault(false) === payload.dashed {
            Result.Ok()
          } else {
            Result.Error([
              "R-Symbol schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched class-ness: " ++
              Bool.toString(slots.is_class->Option.getWithDefault(false)) ++
              " vs " ++
              Bool.toString(payload.dashed),
            ])
          }
          [conceptValid, graphicValid, dashedValid]->Result.allUnit(Array.concatMany)
        }
      | (InspectorState.Schema.Placeholder(slots), ModelNode.Kind.Placeholder) => {
          let payload = ModelNode.payload(node)
          if slots.description === payload.name {
            Result.Ok()
          } else {
            Result.Error([
              "Placeholder schema (" ++
              Gid.toString(ModelNode.id(node)) ++
              ") has mismatched value: " ++
              slots.description ++
              " vs " ++
              payload.name,
            ])
          }
        }
      | _ => Result.Error(["Graphical node and slots do not agree on schema kind!"])
      }

    let checkLinkMatches = (slots, link) =>
      switch (slots, link->ModelLink.kind) {
      | (InspectorState.Link.Hierarchy(slots), ModelLink.Kind.Hierarchy) => {
          let label = link->ModelLink.payload->Option.flatMap(ModelLink.Payload.label)
          if slots.order == label {
            Result.Ok()
          } else {
            Result.Error([
              "Hierarchical link (" ++
              link->ModelLink.id->Gid.toString ++
              ") has mismatched label: " ++
              Option.toString(slots.order, Int.toString) ++
              " vs " ++
              Option.toString(label, Int.toString),
            ])
          }
        }
      | (InspectorState.Link.Anchor(slots), ModelLink.Kind.Anchor) => {
          let label = link->ModelLink.payload->Option.flatMap(ModelLink.Payload.label)
          if slots.order == label {
            Result.Ok()
          } else {
            Result.Error([
              "Anchor link (" ++
              link->ModelLink.id->Gid.toString ++
              ") has mismatched label: " ++
              Option.toString(slots.order, Int.toString) ++
              " vs " ++
              Option.toString(label, Int.toString),
            ])
          }
        }

      | (InspectorState.Link.Generic(_), ModelLink.Kind.Generic) => Result.Ok()
      | _ => Result.Error(["Graphical link and slots do not agree on link kind!"])
      }

    let infoValid = InspectorState.isValid(InspectorState.Global(t.info))
    let graphValid = ModelState.isValid(t.graph)
    let slotsValid =
      t.slots
      ->Gid.Map.toArray
      ->Array.map(((id, slots)) =>
        switch slots {
        | InspectorState.SchemaOrLink.Schema(slots) =>
          [
            InspectorState.isValid(InspectorState.Schema(id, slots)),
            t.graph
            ->ModelState.nodeWithId(id)
            ->Option.map(checkSchemaMatches(slots, _))
            ->Option.getWithDefault(
              Result.Error(["Graph has no schema with ID " ++ Gid.toString(id)]),
            ),
          ]->Result.allUnit(Array.concatMany)
        | InspectorState.SchemaOrLink.Link(slots) =>
          [
            InspectorState.isValid(InspectorState.Link(id, slots)),
            t.graph
            ->ModelState.linkWithId(id)
            ->Option.map(checkLinkMatches(slots, _))
            ->Option.getWithDefault(
              Result.Error(["Graph has no link with ID " ++ Gid.toString(id)]),
            ),
          ]->Result.allUnit(Array.concatMany)
        }
      )
      ->Result.allUnit(Array.concatMany)
    let allNodesHaveSlots =
      t.graph
      ->ModelState.graph
      ->ModelGraph.nodes
      ->Array.map(node =>
        if t.slots->Gid.Map.has(ModelNode.id(node)) {
          Result.Ok()
        } else {
          Result.Error(["Graph node has no associated slots: " ++ Gid.toString(ModelNode.id(node))])
        }
      )
      ->Result.allUnit(Array.concatMany)
    let allLinksHaveSlots =
      t.graph
      ->ModelState.graph
      ->ModelGraph.links
      ->Array.map(link =>
        if t.slots->Gid.Map.has(ModelLink.id(link)) {
          Result.Ok()
        } else {
          Result.Error(["Graph link has no associated slots: " ++ Gid.toString(ModelLink.id(link))])
        }
      )
      ->Result.allUnit(Array.concatMany)

    [infoValid, graphValid, slotsValid, allNodesHaveSlots, allLinksHaveSlots]->Result.allUnit(
      Array.concatMany,
    )
  }
}

let setDB = (newDB, store) => db->SetOnce.set((newDB, store))

type t = {
  models: Gid.Map.t<UndoRedo.t<Model.t>>,
  positions: FileTree.t<Gid.t>,
  currentModel: option<Gid.t>,
  viewTransforms: Gid.Map.t<ReactD3Graph.Graph.ViewTransform.t>,
}

let store = t => {
  LocalStorage.Raw.setItem(
    "RepNotation:CurrentModel",
    t.currentModel->Option.toJson(Gid.toJson)->Js.Json.stringify,
  )
  LocalStorage.Raw.setItem(
    "RepNotation:AllModels",
    t.positions->FileTree.Stable.V2.toJson(Gid.toJson)->Js.Json.stringify,
  )
  t.models->Gid.Map.forEach((id, model) => Model.store(model->UndoRedo.state, id))
}

let load = (~atTime) => {
  let currentModel = LocalStorage.Raw.getItem("RepNotation:CurrentModel")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored currentModel")
    }
    json->Or_error.flatMap(json => json->Option.fromJson(Gid.fromJson))->Or_error.toOption
  })
  let positions = LocalStorage.Raw.getItem("RepNotation:AllModels")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored allModels")
    }
    json
    ->Or_error.flatMap(json => json->FileTree.Stable.V2.fromJson(Gid.fromJson))
    ->Or_error.toOption
  })
  let models =
    positions
    ->Option.map(positions => {
      positions
      ->FileTree.flatten
      ->Array.map(id =>
        Model.load(id)
        ->Promise.thenResolve(m => (id, m))
        ->Promise.catch(e => {
          Js.Console.log3("model.load error", id, e)
          Dialog.alert("Failed to load model with ID: " ++ Gid.toString(id))
          Promise.reject(e)
        })
      )
      ->Promise.all
      ->Promise.thenResolve(arr =>
        arr
        ->Array.keepMap(((id, model)) =>
          switch model->Or_error.match {
          | Or_error.Ok(m) => (id, UndoRedo.create(m, ~atTime))->Some
          | Or_error.Err(e) => {
              Dialog.alert("Error loading model: " ++ Error.messages(e)->Js.Array2.joinWith(";"))
              None
            }
          }
        )
        ->Gid.Map.fromArray
        ->Some
      )
      ->Promise.catch(e => {
        Js.Console.log2("State.load model all", e)
        Dialog.alert("Failed to load all models.")
        Promise.reject(e)
      })
    })
    ->Option.getWithDefault(Promise.resolve(None))

  models->Promise.thenResolve(models => {
    Option.both3((currentModel, positions, models))->Option.map(((
      currentModel,
      positions,
      models,
    )) => {
      models: models,
      positions: positions,
      currentModel: currentModel,
      viewTransforms: Gid.Map.empty(),
    })
  })
}

let toJson = t => {
  Js.Dict.fromArray([
    ("currentModel", t.currentModel->Option.toJson(Gid.toJson)),
    ("positions", t.positions->FileTree.Stable.V2.toJson(Gid.toJson)),
    ("models", t.models->Gid.Map.toJson(model => Model.Stable.V5.toJson(model->UndoRedo.state))),
    ("viewTransforms", t.viewTransforms->Gid.Map.toJson(ViewTransform.Stable.V1.toJson)),
  ])->Js.Json.object_
}
let fromJson = (json, ~atTime) =>
  json
  ->Js.Json.decodeObject
  ->Or_error.fromOption_s("Failed to decode Model state object JSON")
  ->Or_error.flatMap(dict => {
    let getValue = (key, reader) =>
      dict
      ->Js.Dict.get(key)
      ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
      ->Or_error.flatMap(reader)
    let currentModel = getValue("currentModel", Option.fromJson(_, Gid.fromJson))
    let positions = getValue("positions", FileTree.Stable.V2.fromJson(_, Gid.fromJson))
    let models = getValue(
      "models",
      Gid.Map.fromJson(_, json =>
        json->Model.Stable.V5.fromJson->Or_error.map(UndoRedo.create(_, ~atTime))
      ),
    )
    let viewTransforms = getValue(
      "viewTransforms",
      Gid.Map.fromJson(_, ViewTransform.Stable.V1.fromJson),
    )
    (currentModel, positions, models, viewTransforms)
    ->Or_error.both4
    ->Or_error.map(((currentModel, positions, models, viewTransforms)) => {
      models: models,
      currentModel: currentModel,
      positions: positions,
      viewTransforms: viewTransforms,
    })
  })

let isValid = t => {
  let positionsValid =
    t.positions
    ->FileTree.flatten
    ->Array.keepMap(id =>
      if t.models->Gid.Map.has(id) {
        None
      } else {
        Some(id)
      }
    )
    ->(
      arr =>
        switch arr {
        | [] => Result.Ok()
        | ids =>
          ids
          ->Array.map(id => "FileTree references unknown model: " ++ Gid.toString(id))
          ->Result.Error
        }
    )
  let modelsValid =
    t.models
    ->Gid.Map.toArray
    ->Array.map(((id, cons)) =>
      if t.positions->FileTree.flatten->Array.includes(id) {
        cons->UndoRedo.state->Model.isValid
      } else {
        Result.Error(["Model found that is not in FileTree: " ++ Gid.toString(id)])
      }
    )
    ->Result.allUnit(Array.concatMany)
  let vtsValid =
    t.viewTransforms
    ->Gid.Map.keys
    ->Array.map(id =>
      if t.models->Gid.Map.has(id) {
        Result.Ok()
      } else {
        Result.Error(["ViewTransform exists for unknown model: " ++ Gid.toString(id)])
      }
    )
    ->Result.allUnit(Array.concatMany)
  [positionsValid, modelsValid, vtsValid]->Result.allUnit(Array.concatMany)
}

let empty = {
  models: Gid.Map.empty(),
  positions: FileTree.empty(),
  currentModel: None,
  viewTransforms: Gid.Map.empty(),
}

let focused = t => t.currentModel

let models = t =>
  t.positions->FileTree.map(id => (id, t.models->Gid.Map.get(id)->Option.getExn->UndoRedo.state))

let model = (t, id) => t.models->Gid.Map.get(id)->Option.map(UndoRedo.state)

let createModel = (t, id, path, ~atTime) => {
  ...t,
  models: t.models->Gid.Map.set(id, Model.create("Model")->UndoRedo.create(~atTime)),
  positions: t.positions->FileTree.insertFile(~path, ~position=-1, id)->Option.getExn,
  currentModel: Some(id),
}

let deleteModel = (t, id) => {
  if db->SetOnce.isSet {
    Model.delete(id)
  }
  let currentModel = if (
    t.currentModel->Option.map(current => current == id)->Option.getWithDefault(false)
  ) {
    None
  } else {
    t.currentModel
  }
  {
    models: t.models->Gid.Map.remove(id),
    positions: t.positions->FileTree.removeFile(id' => id' === id),
    currentModel: currentModel,
    viewTransforms: t.viewTransforms->Gid.Map.remove(id),
  }
}

let createFolder = (t, id, path) => {
  {
    ...t,
    currentModel: Some(id),
    positions: t.positions
    ->FileTree.newFolder(~path, ~position=-1, ~name="Folder", ~id)
    ->Option.getExn,
  }
}

let deleteFolder = (t, id) => {
  let (positions, removed) = t.positions->FileTree.removeFolderAndContents(id)
  let currentModel = if (
    t.currentModel
    ->Option.map(current => removed->Array.includes(current))
    ->Option.getWithDefault(false)
  ) {
    None
  } else {
    t.currentModel
  }
  {
    ...t,
    models: removed->Array.reduce(t.models, (models, rem) => {
      Model.delete(rem)
      models->Gid.Map.remove(rem)
    }),
    currentModel: currentModel,
    positions: positions,
  }
}
let focusModel = (t, id) => {
  ...t,
  currentModel: id,
}

let duplicateModel = (t, ~existing, ~new_, ~atTime) => {
  let (path, position) =
    t.positions->FileTree.getFilePathAndPosition(id => id === existing)->Option.getExn
  let oldModel = t.models->Gid.Map.get(existing)->Option.getExn->UndoRedo.state
  let newModel =
    oldModel->Model.duplicate(oldModel.info.name ++ " (Copy)")->UndoRedo.create(~atTime)
  {
    ...t,
    currentModel: Some(new_),
    positions: t.positions->FileTree.insertFile(~path, ~position=position + 1, new_)->Option.getExn,
    models: t.models->Gid.Map.set(new_, newModel),
  }
}

let importModel = (t, newId, model, path, ~atTime) => {
  // Duplicate the model to ensure fresh ids
  let model = Model.duplicate(model, model.info.name)->UndoRedo.create(~atTime)
  {
    ...t,
    currentModel: Some(newId),
    positions: t.positions->FileTree.insertFile(~path, ~position=-1, newId)->Option.getExn,
    models: t.models->Gid.Map.set(newId, model),
  }
}

let reorderModels = (t, newOrder) => {
  ...t,
  positions: newOrder,
}

let renameFolder = (t, id, name) => {
  ...t,
  positions: t.positions->FileTree.renameFolder(id, name),
}

let updateModel = (t, id, f, ~atTime) => {
  ...t,
  models: t.models->Gid.Map.update(id, model =>
    model->Option.map(model => {
      let newModel = model->UndoRedo.state->f
      model->UndoRedo.step(newModel, ~atTime)
    })
  ),
}

let updateModelBypassUndoRedo = (t, id, f) => {
  ...t,
  models: t.models->Gid.Map.update(id, model =>
    model->Option.map(model => {
      let newModel = model->UndoRedo.state->f
      model->UndoRedo.replace(newModel)
    })
  ),
}

let undo = (t, id) => {
  ...t,
  models: t.models->Gid.Map.update(id, model => model->Option.map(model => model->UndoRedo.undo)),
}

let redo = (t, id) => {
  ...t,
  models: t.models->Gid.Map.update(id, model => model->Option.map(model => model->UndoRedo.redo)),
}

let canUndo = (t, id) =>
  t.models->Gid.Map.get(id)->Option.map(UndoRedo.canUndo)->Option.getWithDefault(false)
let canRedo = (t, id) =>
  t.models->Gid.Map.get(id)->Option.map(UndoRedo.canRedo)->Option.getWithDefault(false)

let viewTransform = (t, id) => t.viewTransforms->Gid.Map.get(id)
let setViewTransform = (t, id, vt) => {...t, viewTransforms: t.viewTransforms->Gid.Map.set(id, vt)}

let hash = t =>
  [
    t.currentModel->Option.hash(Gid.hash),
    t.models
    ->Gid.Map.toArray
    ->Array.map(((id, model)) => Hash.combine([Gid.hash(id), model->UndoRedo.state->Model.hash]))
    ->Hash.combine,
    t.positions->FileTree.hash(Gid.hash),
  ]->Hash.combine

let removePhantomEdges = t => {
  ...t,
  models: t.models->Gid.Map.map(model =>
    model->UndoRedo.replace(
      model
      ->UndoRedo.state
      ->(
        model => {
          ...model,
          Model.slots: model.Model.slots->Gid.Map.mapPartial((id, slots) =>
            switch slots {
            | InspectorState.SchemaOrLink.Link(slots) =>
              model.graph
              ->ModelState.linkWithId(id)
              ->Option.map(_ => InspectorState.SchemaOrLink.Link(slots))
            | _ => Some(slots)
            }
          ),
        }
      ),
    )
  ),
}
