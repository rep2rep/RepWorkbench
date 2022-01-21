@react.component
let make = (~id, ~data) => {
  <div className="inspector-panel" id>
    {switch data {
    | InspectorState.Empty =>
      <span className="inspector-panel-empty-message"> {React.string("Select a schema")} </span>
    | InspectorState.MultipleSchema =>
      <span className="inspector-panel-multiple-message">
        {React.string("Multiple schema selected")}
      </span>
    | x => {
        Js.Console.log(x)
        React.string("Stuff will go here!")
      }
    }}
  </div>
}
