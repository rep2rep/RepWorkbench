module Model = {
  type t = {
    name: string,
    model: ModelState.t,
    slots: Uuid.Map.t<InspectorState.schema>,
  }

  let model = t => t.model
}

type t = {
  models: array<Model.t>,
  currentModel: option<int>,
}

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
