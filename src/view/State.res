module Model = {
  module T = {
    type t = {
      id: Uuid.t,
      name: string,
      model: ModelState.t,
      slots: Uuid.Map.t<InspectorState.Schema.t>,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("id", t.id->Uuid.toJson),
        ("name", t.name->String.toJson),
        ("model", t.model->ModelState.toJson),
        ("slots", t.slots->Uuid.Map.toJson(InspectorState.Schema.toJson)),
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
        let model = getValue("model", ModelState.fromJson)
        let slots = getValue("slots", j => j->Uuid.Map.fromJson(InspectorState.Schema.fromJson))

        Or_error.both4((id, name, model, slots))->Or_error.map(((id, name, model, slots)) => {
          id: id,
          name: name,
          model: model,
          slots: slots,
        })
      })
  }
  include T

  module Storage = LocalStorage.MakeJsonable(T)

  let store = t => Storage.set("RepNotation:Model:" ++ Uuid.toString(t.id), t)
  let load = id => Storage.get("RepNotation:Model:" ++ Uuid.toString(id))

  let id = t => t.id
  let model = t => t.model
  let name = t => t.name

  let create = (id, name) => {
    id: id,
    name: name,
    model: ModelState.empty,
    slots: Uuid.Map.empty(),
  }

  let duplicate = (t, newId, newName) => {
    let newIdMap = t.slots->Uuid.Map.map(_ => Uuid.create())
    {
      id: newId,
      name: newName,
      model: t.model->ModelState.duplicate(newIdMap),
      slots: t.slots
      ->Uuid.Map.toArray
      ->Array.map(((id, slots)) => (newIdMap->Uuid.Map.get(id)->Option.getExn, slots))
      ->Uuid.Map.fromArray,
    }
  }
}

type t = {
  models: array<Model.t>,
  currentModel: option<Uuid.t>,
}

let store = t => {
  LocalStorage.Raw.setItem(
    "RepNotation:CurrentModel",
    t.currentModel->Option.toJson(Uuid.toJson)->Js.Json.stringify,
  )
  LocalStorage.Raw.setItem(
    "RepNotation:AllModels",
    t.models->Array.map(Model.id)->Array.toJson(Uuid.toJson)->Js.Json.stringify,
  )
  t.models->Array.forEach(Model.store)
}

let load = () => {
  let currentModel = LocalStorage.Raw.getItem("RepNotation:CurrentModel")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored currentModel")
    }
    json->Or_error.flatMap(json => json->Option.fromJson(Uuid.fromJson))->Or_error.toOption
  })
  let models = LocalStorage.Raw.getItem("RepNotation:AllModels")->Option.flatMap(s => {
    let json = try Or_error.create(Js.Json.parseExn(s)) catch {
    | _ => Or_error.error_s("Badly stored allModels")
    }
    json
    ->Or_error.flatMap(json => json->Array.fromJson(Uuid.fromJson))
    ->Or_error.map(arr => arr->Array.map(id => Model.load(id)->Or_error.toOption))
    ->Or_error.map(Option.all)
    ->Or_error.toOption
    ->Option.flatten
  })
  Option.both((currentModel, models))->Option.map(((currentModel, models)) => {
    models: models,
    currentModel: currentModel,
  })
}

let dump = t => {
  Js.Dict.fromList(list{
    ("RepNotation:CurrentModel", t.currentModel->Option.toJson(Uuid.toJson)),
    ("RepNotation:AllModels", t.models->Array.map(Model.id)->Array.toJson(Uuid.toJson)),
    ("model_data", t.models->Array.toJson(Model.toJson)),
  })->Js.Json.object_
}

let empty = {
  models: [],
  currentModel: None,
}

let currentModel = t =>
  t.models->Array.find(model =>
    t.currentModel->Option.map(id => model.id == id)->Option.getWithDefault(false)
  )

let focusedName = t => t->currentModel->Option.map(Model.name)

let focusedId = t => t.currentModel

let models = t => t.models

let createModel = (t, id) => {
  models: Array.concat(t.models, [Model.create(id, "Model")]),
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
    models: t.models->Array.filter(m => m.id != id),
    currentModel: currentModel,
  }
}

let focusModel = (t, id) => {
  ...t,
  currentModel: Some(id),
}

let duplicateModel = (t, ~existing, ~new_) => {
  let dupIndex = t.models->Array.getIndexBy(model => model.id === existing)->Option.getExn
  let oldModel = t.models->Array.getExn(dupIndex)
  let newModel = oldModel->Model.duplicate(new_, oldModel.name ++ " (Copy)")
  let before = t.models->Array.slice(~offset=0, ~len=dupIndex + 1)
  let after = t.models->Array.sliceToEnd(dupIndex + 1)
  {
    currentModel: Some(new_),
    models: Array.concatMany([before, [newModel], after]),
  }
}

let renameModel = (t, id, name) => {
  ...t,
  models: t.models->Array.map(m =>
    if m.id == id {
      {...m, name: name}
    } else {
      m
    }
  ),
}

let modelState = t =>
  t->currentModel->Option.map(Model.model)->Option.getWithDefault(ModelState.empty)

let inspectorState = t =>
  t
  ->currentModel
  ->Option.flatMap(model => {
    let selection = model.model->ModelState.selection
    let getSlot = id => model.slots->Uuid.Map.get(id)
    switch ModelSelection.nodes(selection) {
    | [] => Some(InspectorState.Empty)
    | [nodeId] => getSlot(nodeId)->Option.map(s => InspectorState.Single(nodeId, s))
    | ids =>
      ids
      ->Array.map(id => getSlot(id)->Option.map(slots => (id, slots)))
      ->Option.all
      ->Option.map(slots => InspectorState.Multiple(slots))
    }
  })
  ->Option.getWithDefault(InspectorState.Empty)

let _set = (arr, id, f) => {
  arr->Array.map(m =>
    if id->Option.map(id => m->Model.id == id)->Option.getWithDefault(false) {
      f(m)
    } else {
      m
    }
  )
}

let _setM = (arr, id, model) => _set(arr, id, oldModel => {...oldModel, Model.model: model})

let _setI = (arr, id, key, inspector) =>
  _set(arr, id, oldModel => {
    ...oldModel,
    Model.slots: oldModel.Model.slots->Uuid.Map.update(key, _ => inspector),
  })

let updateModel = (t, model) => {
  ...t,
  models: t.models->_setM(t.currentModel, model),
}

let updateSlots = (t, key, inspector) => {
  ...t,
  models: t.models->_setI(t.currentModel, key, inspector),
}
