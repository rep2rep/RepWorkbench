module Model: {
  type t = {
    name: string,
    model: ModelState.t,
    slots: Belt.Map.String.t<InspectorState.schema>,
  }
}

type t = {
  models: array<Model.t>,
  currentModel: option<int>,
}

let modelState: t => ModelState.t
let inspectorState: t => InspectorState.t

let updateModel: (t, ModelState.t) => t
let updateSlots: (t, string, InspectorState.schema) => t
