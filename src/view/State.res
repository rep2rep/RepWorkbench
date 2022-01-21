module Model = {
  type t = {
    name: string,
    model: ModelState.t,
    slots: Belt.Map.String.t<InspectorState.t>,
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
      | [nodeIdS] => model.slots->Belt.Map.String.get(ReactD3Graph.Node.Id.toString(nodeIdS))
      | [] => Some(InspectorState.Empty)
      | _ => Some(InspectorState.MultipleSchema)
      }
    })
  )
  ->Option.getWithDefault(InspectorState.Empty)

let _set = (arr, i, model) => {
  let i = Option.getExn(i)
  let oldModel = arr[i]->Option.getExn
  let newModel = {...oldModel, Model.model: model}
  let arr = arr->Array.sliceToEnd(0)
  let _ = arr->Array.set(i, newModel)
  arr
}

let updateModel = (t, model) => {
  ...t,
  models: t.models->_set(t.currentModel, model),
}
