module Model: {
  type t = {
    name: string,
    model: ModelState.t,
    slots: Belt.Map.String.t<InspectorState.t>,
  }
}

type t = {
  models: array<Model.t>,
  currentModel: option<int>,
}

let modelState: t => ModelState.t
let inspectorState: t => InspectorState.t

let updateModel: (t, ModelState.t) => t
