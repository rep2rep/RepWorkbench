type timestamp = float
type global = {
  mutable isRecording: bool,
  mutable actions: array<(timestamp, Event.t)>,
  mutable start: State.t,
  mutable epoch: timestamp,
}

let _global = {
  isRecording: false,
  actions: [],
  start: State.empty,
  epoch: 0.,
}

type performance
@val external performance: performance = "performance"
@send external perfNow: performance => float = "now"

type t = (State.t, array<(timestamp, Event.t)>)

let start = ((start, _)) => start
let events = ((_, events)) => events

let toJson = ((start, events)) => {
  let eventToJson = ((timestamp, event)) => {
    let ts_j = Float.toJson(timestamp)
    let ev_j = Event.toJson(event)
    [ts_j, ev_j]->Array.toJson(j => j)
  }
  [
    String.toJson("##VERSION##"),
    State.toJson(start),
    events->Array.toJson(eventToJson),
  ]->Array.toJson(j => j)
}

let fromJson = json =>
  switch json->Array.fromJson(Or_error.create)->Or_error.match {
  | Or_error.Ok([version, start_json, events_json]) => {
      let version = String.fromJson(version)
      switch version->Or_error.match {
      | Or_error.Ok("##VERSION##") => {
          let start = State.fromJson(start_json)
          let eventFromJson = json =>
            switch json->Array.fromJson(Or_error.create)->Or_error.match {
            | Or_error.Ok([ts_j, ev_j]) => {
                let timestamp = Float.fromJson(ts_j)
                let event = Event.fromJson(ev_j)
                (timestamp, event)->Or_error.both
              }
            | Or_error.Ok(_) => Or_error.error_s("Recording Record is malformed")
            | Or_error.Err(e) => Or_error.error(e)
            }
          let events = events_json->Array.fromJson(eventFromJson)
          (start, events)->Or_error.both
        }
      | _ => {
          Dialog.alert("Bad recording version, ask Aaron to produce a new viewer for you!")
          Or_error.error_s("Bad recording version")
        }
      }
    }
  | Or_error.Ok(_) => Or_error.error_s("Recording JSON malformed.")
  | Or_error.Err(e) => Or_error.error(e)
  }

let startRecording = state =>
  if !_global.isRecording {
    Js.Console.log("Started recording...")
    _global.isRecording = true
    _global.actions = []
    _global.start = state
    _global.epoch = perfNow(performance)
  }

let stopRecording = () => {
  Js.Console.log("Stopped recording.")
  let start = _global.start
  let events = _global.actions->Array.copy
  _global.isRecording = false
  _global.actions = []
  _global.start = State.empty
  _global.epoch = 0.
  (start, events)
}

let isRecording = () => _global.isRecording
let record = event =>
  _global.actions->Js.Array2.push((perfNow(performance) -. _global.epoch, event))->ignore

let unwind = ((start, events)) => {
  let result = [(0., start)]
  let now = ref(start)
  events->Array.forEach(((time, event)) => {
    now := Event.dispatch(now.contents, event, ~atTime=time)
    result->Js.Array2.push((time, now.contents))->ignore
  })
  result
}
