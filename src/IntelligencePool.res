type t = {
  script: string,
  mutable running: array<Intelligence_Intf.Request.t>,
  mutable waiting: Gid.Map.t<Intelligence_Intf.Request.t>,
  mutable waitingOrder: array<Gid.t>,
  mutable listeners: array<Intelligence_Intf.Response.t => unit>,
  limit: int,
}

let _TIMEOUT = 300000 // 5 minutes

@val external window: {..} = "window"

let create = script => {
  script: script,
  running: [],
  waiting: Gid.Map.empty(),
  waitingOrder: [],
  listeners: [],
  limit: window["navigator"]["hardwareConcurrency"]
  ->Js.Nullable.toOption
  ->Option.getWithDefault(6) - 1,
}

let rec mkWorker = (t, request: Intelligence_Intf.Request.t) => {
  let worker = Intelligence_Intf.create(t.script)
  let lastResponse = ref(None)
  let timeout = Js.Global.setTimeout(() => {
    Intelligence_Intf.terminate(worker)
    lastResponse.contents->Option.iter((response: Intelligence_Intf.Response.t) => {
      let resp = {
        ...response,
        killed: true,
      }
      t.listeners->Array.forEach(f => f(resp))
    })
    t.running = t.running->Array.keep(r => r !== request)
    nextWorker(t)
  }, _TIMEOUT)
  t.running->Js.Array2.push(request)->ignore
  worker->Intelligence_Intf.listen((response: Intelligence_Intf.Response.t) => {
    lastResponse := Some(response)
    t.listeners->Array.forEach(f => f(response))
    if response.errors_done && response.warnings_done && response.insights_done {
      Js.Global.clearTimeout(timeout)
      Intelligence_Intf.terminate(worker)
      t.running = t.running->Array.keep(r => r !== request)
      nextWorker(t)
    }
  })
  worker->Intelligence_Intf.post(request)
}
and nextWorker = t => {
  let model = ref(None)
  while model.contents === None && t.waitingOrder->Array.length > 0 {
    model :=
      t.waitingOrder
      ->Js.Array2.shift
      ->Option.flatMap(id => {
        t.waiting->Gid.Map.get(id)->Option.map(req => (id, req))
      })
  }
  model.contents->Option.iter(((id, req)) => {
    mkWorker(t, req)
    t.waiting = t.waiting->Gid.Map.remove(id)
  })
}

let post = (t, request: Intelligence_Intf.Request.t) => {
  if Array.length(t.running) > t.limit {
    t.waiting = t.waiting->Gid.Map.set(request.model, request)
    t.waitingOrder->Js.Array2.push(request.model)->ignore
  } else {
    mkWorker(t, request)
  }
}

let listen = (t, f) => t.listeners->Js.Array2.push(f)->ignore
