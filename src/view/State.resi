module Model: {
  type t = {
    name: string,
    model: ModelState.t,
    slots: Uuid.Map.t<InspectorState.schema>,
  }
}

type t = {
  models: array<Model.t>,
  currentModel: option<int>,
}

let modelState: t => ModelState.t
let inspectorState: t => InspectorState.t

let updateModel: (t, ModelState.t) => t
let updateSlots: (t, Uuid.t, option<InspectorState.schema>) => t
