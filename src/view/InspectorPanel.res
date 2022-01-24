module Representation = {
  @react.component
  let make = (~slots: InspectorState.Representation.t, ~onChange) => {
    <>
      <div className="inspect-rep-row">
        <label htmlFor="inspector-rep-domain"> {React.string("Domain")} </label>
        <input
          type_="text"
          value={slots.domain}
          name="inspector-rep-domain"
          onChange={e =>
            onChange(InspectorEvent.Representation.Domain(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-rep-row">
        <label htmlFor="inspector-rep-display"> {React.string("Display")} </label>
        <input
          type_="text"
          value={slots.display}
          name="inspector-rep-display"
          onChange={e =>
            onChange(InspectorEvent.Representation.Display(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-rep-row">
        <label htmlFor="inspector-rep-notes"> {React.string("Notes")} </label>
      </div>
      <div className="inspect-rep-row">
        <textarea
          name="inspector-rep-notes"
          onChange={e =>
            onChange(InspectorEvent.Representation.Notes(ReactEvent.Form.target(e)["value"]))}>
          {React.string(slots.notes)}
        </textarea>
      </div>
    </>
  }
}

@react.component
let make = (~id, ~data, ~onChange=?) => {
  let onChange = onChange->Option.getWithDefault(_ => ())
  <div className="inspector-panel" id>
    {switch data {
    | InspectorState.Empty =>
      <span className="inspector-panel-empty-message"> {React.string("Select a schema")} </span>
    | InspectorState.Multiple =>
      <span className="inspector-panel-multiple-message">
        {React.string("Multiple schema selected")}
      </span>
    | InspectorState.Single(schema) =>
      switch schema {
      | InspectorState.Schema.Representation(slots) =>
        <Representation slots onChange={c => onChange(InspectorEvent.Representation(c))} />
      | _ => React.string("Stuff will go here!")
      }
    }}
  </div>
}
