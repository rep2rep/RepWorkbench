module T = Intelligence_Intf.WorkerThread

T.create(request => {
  Js.Console.log(("Received model!", request))
  {errors: [], warnings: []}
})
