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
      type t = t = {
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
          let schemas = t.V3.slots->Gid.Map.map(s => InspectorState.SchemaOrLink.Schema(s))
          let links =
            graph
            ->ModelState.graph
            ->ModelGraph.links
            ->Array.map(link => (
              ModelLink.id(link),
              InspectorState.SchemaOrLink.Link(InspectorState.Link.empty(ModelLink.kind(link))),
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
              let slots = getValue("slots", j =>
                j->Gid.Map.fromJson(InspectorState.SchemaOrLink.Stable.V1.fromJson)
              )

              Or_error.both3((info, graph, slots))->Or_error.map(((info, graph, slots)) => {
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
        ->Promise.catch(_ => {
          Dialog.alert("Failed to save! Couldn't write data")
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
        ->Promise.catch(_ => {
          Js.Console.log("Failed to load model " ++ key ++ ", checking LocalStorage.")
          let extant = OldStorage.get(key)
          if Or_error.isOk(extant) {
            Js.Console.log("Loaded " ++ key ++ " from LocalStorage. Removing old version.")
            OldStorage.delete(key)
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
          Js.Console.log(e)
          Dialog.alert("Failed to delete model!")
          Promise.resolve()
        })
        ->ignore
      }
    }
  }
  module Storage = StorageMkr(Stable.V4)

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
}

let setDB = (newDB, store) => db->SetOnce.set((newDB, store))

type t = {
  models: Gid.Map.t<UndoRedo.t<Model.t>>,
  positions: array<Gid.t>,
  currentModel: option<Gid.t>,
}

let store = t => {
  LocalStorage.Raw.setItem(
    "RepNotation:CurrentModel",
    t.currentModel->Option.toJson(Gid.toJson)->Js.Json.stringify,
  )
  LocalStorage.Raw.setItem(
    "RepNotation:AllModels",
    t.positions->Array.toJson(Gid.toJson)->Js.Json.stringify,
  )
  t.models->Gid.Map.forEach((id, model) => Model.store(model->UndoRedo.state, id))
}

let load = () => {
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
    json->Or_error.flatMap(json => json->Array.fromJson(Gid.fromJson))->Or_error.toOption
  })
  let models =
    positions
    ->Option.map(positions => {
      positions
      ->Array.map(id => Model.load(id)->Promise.thenResolve(m => (id, m)))
      ->Promise.all
      ->Promise.thenResolve(arr =>
        arr
        ->Array.keepMap(((id, model)) =>
          switch model->Or_error.match {
          | Or_error.Ok(m) => (id, UndoRedo.create(m))->Some
          | Or_error.Err(e) => {
              Dialog.alert("Error loading model: " ++ Error.messages(e)->Js.Array2.joinWith(";"))
              None
            }
          }
        )
        ->Gid.Map.fromArray
        ->Some
      )
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
    })
  })
}

let empty = {
  models: Gid.Map.empty(),
  positions: [],
  currentModel: None,
}

let focused = t => t.currentModel

let models = t =>
  t.positions->Array.map(id => (id, t.models->Gid.Map.get(id)->Option.getExn->UndoRedo.state))

let model = (t, id) => t.models->Gid.Map.get(id)->Option.map(UndoRedo.state)

let createModel = (t, id) => {
  models: t.models->Gid.Map.set(id, Model.create("Model")->UndoRedo.create),
  positions: t.positions->Array.concat([id]),
  currentModel: Some(id),
}

let deleteModel = (t, id) => {
  Model.delete(id)
  let currentModel = if (
    t.currentModel->Option.map(current => current == id)->Option.getWithDefault(false)
  ) {
    None
  } else {
    t.currentModel
  }
  {
    models: t.models->Gid.Map.remove(id),
    positions: t.positions->Array.filter(id' => id' !== id),
    currentModel: currentModel,
  }
}

let focusModel = (t, id) => {
  ...t,
  currentModel: id,
}

let duplicateModel = (t, ~existing, ~new_) => {
  let dupIndex = t.positions->Array.getIndexBy(id => id === existing)->Option.getExn
  let oldModel = t.models->Gid.Map.get(existing)->Option.getExn->UndoRedo.state
  let newModel = oldModel->Model.duplicate(oldModel.info.name ++ " (Copy)")->UndoRedo.create
  let before = t.positions->Array.slice(~offset=0, ~len=dupIndex + 1)
  let after = t.positions->Array.sliceToEnd(dupIndex + 1)
  {
    currentModel: Some(new_),
    positions: Array.concatMany([before, [new_], after]),
    models: t.models->Gid.Map.set(new_, newModel),
  }
}

let importModel = (t, model) => {
  // Duplicate the model to ensure fresh ids
  let newId = Gid.create()
  let model = Model.duplicate(model, model.info.name)->UndoRedo.create
  {
    currentModel: Some(newId),
    positions: t.positions->Array.concat([newId]),
    models: t.models->Gid.Map.set(newId, model),
  }
}

let reorderModels = (t, newOrder) => {
  ...t,
  positions: newOrder,
}

let updateModel = (t, id, f) => {
  ...t,
  models: t.models->Gid.Map.update(id, model =>
    model->Option.map(model => {
      let newModel = model->UndoRedo.state->f
      model->UndoRedo.step(newModel)
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
