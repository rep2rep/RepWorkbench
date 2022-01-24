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
            <option value={Function.toString(f)} key={Function.toString(f)}>
              {React.string(Function.toString(f))}
            </option>
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
          ->Array.map(s =>
            <option value={Scope.toString(s)} key={Scope.toString(s)}>
              {React.string(Scope.toString(s))}
            </option>
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

module Dimension = {
  @react.component
  let make = (~slots: InspectorState.Dimension.t, ~onChange) => {
    <>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-concept"> {React.string("Concept")} </label>
        <input
          type_="text"
          value={slots.concept}
          name="inspector-dim-concept"
          onChange={e =>
            onChange(InspectorEvent.Dimension.Concept(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-concept-scale"> {React.string("Concept Scale")} </label>
        <select
          name="inspector-dim-concept-scale"
          value={Quantity_scale.toString(slots.concept_scale)}
          onChange={e =>
            onChange(
              InspectorEvent.Dimension.Concept_scale(
                Quantity_scale.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Quantity_scale.all
          ->Array.map(f =>
            <option value={Quantity_scale.toString(f)} key={Quantity_scale.toString(f)}>
              {React.string(Quantity_scale.toString(f))}
            </option>
          )
          ->React.array}
        </select>
      </div>
      // Need to add concept attributes
      <div className="inspect-row">
        <label htmlFor="inspector-dim-graphic"> {React.string("Graphic")} </label>
        <input
          type_="text"
          value={slots.graphic}
          name="inspector-dim-graphic"
          onChange={e =>
            onChange(InspectorEvent.Dimension.Graphic(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-graphic-scale"> {React.string("Graphic Scale")} </label>
        <select
          name="inspector-dim-graphic-scale"
          value={Quantity_scale.toString(slots.graphic_scale)}
          onChange={e =>
            onChange(
              InspectorEvent.Dimension.Graphic_scale(
                Quantity_scale.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Quantity_scale.all
          ->Array.map(f =>
            <option value={Quantity_scale.toString(f)} key={Quantity_scale.toString(f)}>
              {React.string(Quantity_scale.toString(f))}
            </option>
          )
          ->React.array}
        </select>
      </div>
      // Need to add graphic attributes
      <div className="inspect-row">
        <label htmlFor="inspector-dim-function"> {React.string("Function")} </label>
        <select
          name="inspector-dim-function"
          value={Function.toString(slots.function)}
          onChange={e =>
            onChange(
              InspectorEvent.Dimension.Function(
                Function.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Function.all
          ->Array.map(f =>
            <option value={Function.toString(f)} key={Function.toString(f)}>
              {React.string(Function.toString(f))}
            </option>
          )
          ->React.array}
        </select>
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-explicit"> {React.string("Explicit")} </label>
        <input
          name="inspector-dim-explicit"
          type_="checkbox"
          checked={slots.explicit}
          onChange={e =>
            onChange(InspectorEvent.Dimension.Explicit(ReactEvent.Form.target(e)["checked"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-scope"> {React.string("Scope")} </label>
        <select
          name="inspector-dim-scope"
          value={Scope.toString(slots.scope)}
          onChange={e =>
            onChange(
              InspectorEvent.Dimension.Scope(
                Scope.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Scope.all
          ->Array.map(s =>
            <option value={Scope.toString(s)} key={Scope.toString(s)}>
              {React.string(Scope.toString(s))}
            </option>
          )
          ->React.array}
        </select>
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-organisation"> {React.string("Organisation")} </label>
        <input
          type_="text"
          value={slots.organisation}
          name="inspector-dim-organisation"
          onChange={e =>
            onChange(InspectorEvent.Dimension.Organisation(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-dim-notes"> {React.string("Notes")} </label>
      </div>
      <div className="inspect-row">
        <textarea
          name="inspector-dim-notes"
          onChange={e =>
            onChange(InspectorEvent.Dimension.Notes(ReactEvent.Form.target(e)["value"]))}
          value={slots.notes}
        />
      </div>
    </>
  }
}

module Token = {
  @react.component
  let make = (~slots: InspectorState.Token.t, ~onChange) => {
    <>
      <div className="inspect-row">
        <label htmlFor="inspector-tok-concept"> {React.string("Concept")} </label>
        <input
          type_="text"
          value={slots.concept}
          name="inspector-tok-concept"
          onChange={e => onChange(InspectorEvent.Token.Concept(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-tok-graphic"> {React.string("Graphic")} </label>
        <input
          type_="text"
          value={slots.graphic}
          name="inspector-tok-graphic"
          onChange={e => onChange(InspectorEvent.Token.Graphic(ReactEvent.Form.target(e)["value"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-tok-class"> {React.string("Is class")} </label>
        <input
          name="inspector-tok-class"
          type_="checkbox"
          checked={slots.is_class}
          onChange={e =>
            onChange(InspectorEvent.Token.Is_class(ReactEvent.Form.target(e)["checked"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-tok-function"> {React.string("Function")} </label>
        <select
          name="inspector-tok-function"
          value={Function.toString(slots.function)}
          onChange={e =>
            onChange(
              InspectorEvent.Token.Function(
                Function.fromString(ReactEvent.Form.target(e)["value"])->Option.getExn,
              ),
            )}>
          {Function.all
          ->Array.map(f =>
            <option value={Function.toString(f)} key={Function.toString(f)}>
              {React.string(Function.toString(f))}
            </option>
          )
          ->React.array}
        </select>
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-tok-explicit"> {React.string("Explicit")} </label>
        <input
          name="inspector-tok-explicit"
          type_="checkbox"
          checked={slots.explicit}
          onChange={e =>
            onChange(InspectorEvent.Token.Explicit(ReactEvent.Form.target(e)["checked"]))}
        />
      </div>
      <div className="inspect-row">
        <label htmlFor="inspector-sch-notes"> {React.string("Notes")} </label>
      </div>
      <div className="inspect-row">
        <textarea
          name="inspector-tok-notes"
          onChange={e => onChange(InspectorEvent.Token.Notes(ReactEvent.Form.target(e)["value"]))}
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
      | InspectorState.Schema.Dimension(slots) =>
        <Dimension slots onChange={c => onChange(InspectorEvent.Dimension(c))} />
      | InspectorState.Schema.Token(slots) =>
        <Token slots onChange={c => onChange(InspectorEvent.Token(c))} />
      }
    }}
  </div>
}
