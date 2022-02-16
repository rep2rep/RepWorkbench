module Model = {
  type t = {
    info: InspectorState.Model.t,
    graph: ModelState.t,
    slots: Uuid.Map.t<InspectorState.Schema.t>,
  }

  module Stable = {
    module V1 = {
      type t = {
        id: Uuid.t,
        name: string,
        model: ModelState.Stable.V1.t,
        slots: Uuid.Map.t<InspectorState.Schema.Stable.V1.t>,
      }

      let toJson = t =>
        Js.Dict.fromList(list{
          ("id", t.id->Uuid.toJson),
          ("name", t.name->String.toJson),
          ("model", t.model->ModelState.Stable.V1.toJson),
          ("slots", t.slots->Uuid.Map.toJson(InspectorState.Schema.Stable.V1.toJson)),
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
          let id = getValue("id", Uuid.fromJson)
          let name = getValue("name", String.fromJson)
          let model = getValue("model", ModelState.Stable.V1.fromJson)
          let slots = getValue("slots", j =>
            j->Uuid.Map.fromJson(InspectorState.Schema.Stable.V1.fromJson)
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
      type t = t = {
        info: InspectorState.Model.Stable.V1.t,
        graph: ModelState.Stable.V2.t,
        slots: Uuid.Map.t<InspectorState.Schema.Stable.V1.t>,
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
          ("slots", t.slots->Uuid.Map.toJson(InspectorState.Schema.Stable.V1.toJson)),
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
              j->Uuid.Map.fromJson(InspectorState.Schema.Stable.V1.fromJson)
            )

            Or_error.both3((info, graph, slots))->Or_error.map(((info, graph, slots)) => {
              info: info,
              graph: graph,
              slots: slots,
            })
          }
        })
    }
  }

  module Storage = LocalStorage.MakeJsonable(Stable.V2)

  let store = (t, id) => Storage.set("RepNotation:Model:" ++ Uuid.toString(id), t)
  let load = id => Storage.get("RepNotation:Model:" ++ Uuid.toString(id))

  let info = t => t.info
  let graph = t => t.graph
  let slots = t => t.slots
  let slotsForSelection = (t, selection) => {
    let ids = ModelSelection.nodes(selection)
    t.slots->Uuid.Map.merge(Uuid.Map.empty(), (key, left, _) =>
      left->Option.flatMap(left =>
        if ids->Array.includes(key) {
          Some(left)
        } else {
          None
        }
      )
    )
  }

  let updateInfo = (t, info) => {...t, info: info}
  let updateGraph = (t, graph) => {...t, graph: graph}
  let updateSlots = (t, slots) => {...t, slots: slots}

  let create = name => {
    info: InspectorState.Model.create(~name),
    graph: ModelState.empty,
    slots: Uuid.Map.empty(),
  }

  let duplicate = (t, newName) => {
    let newIdMap = t.slots->Uuid.Map.map(_ => Uuid.create())
    {
      info: {...t.info, name: newName},
      graph: t.graph->ModelState.duplicate(newIdMap),
      slots: t.slots
      ->Uuid.Map.toArray
      ->Array.map(((id, slots)) => (newIdMap->Uuid.Map.get(id)->Option.getExn, slots))
      ->Uuid.Map.fromArray,
    }
  }
}

type t = {
  models: Uuid.Map.t<UndoRedo.t<Model.t>>,
  positions: array<Uuid.t>,
  currentModel: option<Uuid.t>,
}

let store = t => {
  LocalStorage.Raw.setItem(
    "RepNotation:CurrentModel",
    t.currentModel->Option.toJson(Uuid.toJson)->Js.Json.stringify,
  )
  LocalStorage.Raw.setItem(
    "RepNotation:AllModels",
    t.positions->Array.toJson(Uuid.toJson)->Js.Json.stringify,
  )
  t.models->Uuid.Map.forEach((id, model) => Model.store(model->UndoRedo.state, id))
}

let load = () => {
  let currentModel = LocalStorage.Raw.getItem("RepNotation:CurrentModel")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored currentModel")
    }
    json->Or_error.flatMap(json => json->Option.fromJson(Uuid.fromJson))->Or_error.toOption
  })
  let positions = LocalStorage.Raw.getItem("RepNotation:AllModels")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored allModels")
    }
    json
    ->Or_error.flatMap(json => json->Array.fromJson(Uuid.fromJson))
    // ->Or_error.map(arr =>
    //   arr->Array.map(id => Model.load(id)->Or_error.toOption->Option.map(m => (id, m)))
    // )
    // ->Or_error.map(Option.all)
    ->Or_error.toOption
    // ->Option.flatten
    // ->Option.map(Uuid.Map.fromArray)
  })
  let models = positions->Option.flatMap(positions => {
    positions
    ->Array.map(id => Model.load(id)->Or_error.toOption->Option.map(m => (id, UndoRedo.create(m))))
    ->Option.all
    ->Option.map(Uuid.Map.fromArray)
  })
  Option.both3((currentModel, positions, models))->Option.map(((
    currentModel,
    positions,
    models,
  )) => {
    models: models,
    positions: positions,
    currentModel: currentModel,
  })
}

let empty = {
  models: Uuid.Map.empty(),
  positions: [],
  currentModel: None,
}

let focused = t => t.currentModel

let models = t =>
  t.positions->Array.map(id => (id, t.models->Uuid.Map.get(id)->Option.getExn->UndoRedo.state))

let model = (t, id) => t.models->Uuid.Map.get(id)->Option.map(UndoRedo.state)

let createModel = (t, id) => {
  models: t.models->Uuid.Map.set(id, Model.create("Model")->UndoRedo.create),
  positions: t.positions->Array.concat([id]),
  currentModel: Some(id),
}

let deleteModel = (t, id) => {
  LocalStorage.Raw.removeItem("RepNotation:Model:" ++ Uuid.toString(id))
  let currentModel = if (
    t.currentModel->Option.map(current => current == id)->Option.getWithDefault(false)
  ) {
    None
  } else {
    t.currentModel
  }
  {
    models: t.models->Uuid.Map.remove(id),
    positions: t.positions->Array.filter(id' => id' !== id),
    currentModel: currentModel,
  }
}

let focusModel = (t, id) => {
  ...t,
  currentModel: Some(id),
}

let duplicateModel = (t, ~existing, ~new_) => {
  let dupIndex = t.positions->Array.getIndexBy(id => id === existing)->Option.getExn
  let oldModel = t.models->Uuid.Map.get(existing)->Option.getExn->UndoRedo.state
  let newModel = oldModel->Model.duplicate(oldModel.info.name ++ " (Copy)")->UndoRedo.create
  let before = t.positions->Array.slice(~offset=0, ~len=dupIndex + 1)
  let after = t.positions->Array.sliceToEnd(dupIndex + 1)
  {
    currentModel: Some(new_),
    positions: Array.concatMany([before, [new_], after]),
    models: t.models->Uuid.Map.set(new_, newModel),
  }
}

let importModel = (t, model) => {
  // Duplicate the model to ensure fresh ids
  let newId = Uuid.create()
  let model = Model.duplicate(model, model.info.name)->UndoRedo.create
  {
    currentModel: Some(newId),
    positions: t.positions->Array.concat([newId]),
    models: t.models->Uuid.Map.set(newId, model),
  }
}

let reorderModels = (t, newOrder) => {
  ...t,
  positions: newOrder,
}

let updateModel = (t, id, newModel) => {
  ...t,
  models: t.models->Uuid.Map.update(id, model =>
    model->Option.map(model => model->UndoRedo.step(newModel))
  ),
}

let undo = (t, id) => {
  ...t,
  models: t.models->Uuid.Map.update(id, model => model->Option.map(model => model->UndoRedo.undo)),
}

let redo = (t, id) => {
  ...t,
  models: t.models->Uuid.Map.update(id, model => model->Option.map(model => model->UndoRedo.redo)),
}

let canUndo = (t, id) =>
  t.models->Uuid.Map.get(id)->Option.map(UndoRedo.canUndo)->Option.getWithDefault(false)
let canRedo = (t, id) =>
  t.models->Uuid.Map.get(id)->Option.map(UndoRedo.canRedo)->Option.getWithDefault(false)
