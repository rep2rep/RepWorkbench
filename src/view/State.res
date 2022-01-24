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

  let create = name => {
    id: Uuid.create(),
    name: name,
    model: ModelState.empty,
    slots: Uuid.Map.empty(),
  }
}

type t = {
  models: array<Model.t>,
  currentModel: option<int>,
}

let store = t => {
  LocalStorage.Raw.setItem(
    "RepNotation:CurrentModel",
    t.currentModel->Option.toJson(Int.toJson)->Js.Json.stringify,
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
    json->Or_error.flatMap(json => json->Option.fromJson(Int.fromJson))->Or_error.toOption
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

// let empty = {
//   models: [],
//   currentModel: None,
// }

// A "non-empty" empty while we don't have multiple models
let empty = {
  models: [Model.create("Model")],
  currentModel: Some(0),
}

let name = t =>
  t.currentModel->Option.flatMap(currentModel => t.models[currentModel]->Option.map(Model.name))

let modelState = t =>
  t.currentModel
  ->Option.flatMap(currentModel => t.models[currentModel]->Option.map(Model.model))
  ->Option.getWithDefault(ModelState.empty)

let inspectorState = t =>
  t.currentModel
  ->Option.flatMap(currentModel =>
    t.models[currentModel]->Option.flatMap(model => {
      let selection = model.model->ModelState.selection
      switch ModelSelection.nodes(selection) {
      | [nodeId] => model.slots->Uuid.Map.get(nodeId)->Option.map(s => InspectorState.Single(s))
      | [] => Some(InspectorState.Empty)
      | _ => Some(InspectorState.Multiple)
      }
    })
  )
  ->Option.getWithDefault(InspectorState.Empty)

let _set = (arr, i, f) => {
  let i = Option.getExn(i)
  let oldModel = arr[i]->Option.getExn
  let newModel = f(oldModel)
  let arr = arr->Array.sliceToEnd(0)
  let _ = arr->Array.set(i, newModel)
  arr
}

let _setM = (arr, i, model) => _set(arr, i, oldModel => {...oldModel, Model.model: model})

let _setI = (arr, i, key, inspector) =>
  _set(arr, i, oldModel => {
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
