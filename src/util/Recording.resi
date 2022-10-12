type t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let startRecording: State.t => unit
let stopRecording: unit => t
let isRecording: unit => bool
let record: Event.t => unit

let start: t => State.t
let events: t => array<(float, Event.t)>

let unwind: t => array<(float, State.t)>
