module Representation = {
  @react.component
  let make = (~slots: InspectorState.Representation.t, ~onChange) => {
    <>
      <div className="inspect-row">
        <label htmlFor="inspector-rep-domain"> {React.string("Domain")} </label>
        <input
          type_="text"
          value={slots.domain}
          name="inspector-rep-domain"
          onChange={e =>
            onChange(InspectorEvent.Representation.Domain(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-rep-display"> {React.string("Display")} </label>
        <input
          type_="text"
          value={slots.display}
          name="inspector-rep-display"
          onChange={e =>
            onChange(InspectorEvent.Representation.Display(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-rep-notes"> {React.string("Notes")} </label>
      </div>
      <div className="inspect-row">
        <textarea
          name="inspector-rep-notes"
          onChange={e =>
            onChange(InspectorEvent.Representation.Notes(ReactEvent.Form.target(e)["value"]))}
          value={slots.notes}
        />
      </div>
    </>
  }
}

module Scheme = {
  @react.component
  let make = (~slots: InspectorState.Scheme.t, ~onChange) => {
    <>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-concept"> {React.string("Concept")} </label>
        <input
          type_="text"
          value={slots.concept_structure}
          name="inspector-sch-concept"
          onChange={e =>
            onChange(InspectorEvent.Scheme.Concept_structure(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-graphic"> {React.string("Graphic")} </label>
        <input
          type_="text"
          value={slots.graphic_structure}
          name="inspector-sch-graphic"
          onChange={e =>
            onChange(InspectorEvent.Scheme.Graphic_structure(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-function"> {React.string("Function")} </label>
        <select
          name="inspector-sch-function"
          value={Function.toString(slots.function)}
          onChange={e =>
            onChange(
              InspectorEvent.Scheme.Function(
                Function.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Function.all
          ->Array.map(f =>
            <option value={Function.toString(f)}> {React.string(Function.toString(f))} </option>
          )
          ->React.array}
        </select>
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-explicit"> {React.string("Explicit")} </label>
        <input
          name="inspector-sch-explicit"
          type_="checkbox"
          checked={slots.explicit}
          onChange={e =>
            onChange(InspectorEvent.Scheme.Explicit(ReactEvent.Form.target(e)["checked"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-scope"> {React.string("Scope")} </label>
        <select
          name="inspector-sch-scope"
          value={Scope.toString(slots.scope)}
          onChange={e =>
            onChange(
              InspectorEvent.Scheme.Scope(
                Scope.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Scope.all
          ->Array.map(f =>
            <option value={Scope.toString(f)}> {React.string(Scope.toString(f))} </option>
          )
          ->React.array}
        </select>
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-organisation"> {React.string("Organisation")} </label>
        <input
          type_="text"
          value={slots.organisation}
          name="inspector-sch-organisation"
          onChange={e =>
            onChange(InspectorEvent.Scheme.Organisation(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-notes"> {React.string("Notes")} </label>
      </div>
      <div className="inspect-row">
        <textarea
          name="inspector-sch-notes"
          onChange={e => onChange(InspectorEvent.Scheme.Notes(ReactEvent.Form.target(e)["value"]))}
          value={slots.notes}
        />
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
      | InspectorState.Schema.Scheme(slots) =>
        <Scheme slots onChange={c => onChange(InspectorEvent.Scheme(c))} />
      | _ => React.string("Stuff will go here!")
      }
    }}
  </div>
}
