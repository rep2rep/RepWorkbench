module Row = {
  @react.component
  let make = (~children) => {
    <div
      style={ReactDOM.Style.make(
        ~margin="0.125rem 0.5rem",
        ~minHeight="20px",
        ~display="flex",
        ~alignItems="center",
        (),
      )}
      className="inspector-row">
      {children}
    </div>
  }
}

module Label = {
  @react.component
  let make = (~htmlFor=?, ~children) => {
    <label style={ReactDOM.Style.make(~fontSize="small", ~marginRight="0.5rem", ())} ?htmlFor>
      {children}
    </label>
  }
}

module Input = {
  @react.component
  let make = (~name=?, ~value=?, ~onChange=?) => {
    <input
      type_="text"
      ?name
      ?value
      ?onChange
      style={ReactDOM.Style.make(
        ~flexGrow="1",
        ~border="1px solid black",
        ~borderRadius="2px",
        ~padding="0.125rem 0.25rem",
        (),
      )}
    />
  }
}

module Notes = {
  @react.component
  let make = (~name, ~value=?, ~onChange=?) => {
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="column",
        ~margin="0.125rem 0.5rem",
        (),
      )}>
      <Label htmlFor={name}> {React.string("Notes")} </Label>
      <textarea
        name
        ?onChange
        ?value
        style={ReactDOM.Style.make(
          ~height="200px",
          ~border="1px solid black",
          ~borderRadius="2px",
          ~padding="0.25rem",
          ~marginTop="0.125rem",
          ~fontSize="small",
          ~fontFamily="sans-serif",
          (),
        )}
      />
    </div>
  }
}

module Representation = {
  @react.component
  let make = (~slots: InspectorState.Representation.t, ~onChange) => {
    <>
      <Row>
        <Label htmlFor="inspector-rep-domain"> {React.string("Domain")} </Label>
        <Input
          value={slots.domain}
          name="inspector-rep-domain"
          onChange={e =>
            onChange(Event.Slots.Representation.Domain(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-rep-display"> {React.string("Display")} </Label>
        <Input
          value={slots.display}
          name="inspector-rep-display"
          onChange={e =>
            onChange(Event.Slots.Representation.Display(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Notes
        name="inspector-rep-notes"
        onChange={e =>
          onChange(Event.Slots.Representation.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Scheme = {
  @react.component
  let make = (~slots: InspectorState.Scheme.t, ~onChange) => {
    <>
      <Row>
        <Label htmlFor="inspector-sch-concept"> {React.string("Concept")} </Label>
        <Input
          value={slots.concept_structure}
          name="inspector-sch-concept"
          onChange={e =>
            onChange(Event.Slots.Scheme.Concept_structure(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-graphic"> {React.string("Graphic")} </Label>
        <Input
          value={slots.graphic_structure}
          name="inspector-sch-graphic"
          onChange={e =>
            onChange(Event.Slots.Scheme.Graphic_structure(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-function"> {React.string("Function")} </Label>
        <select
          name="inspector-sch-function"
          value={Function.toString(slots.function)}
          onChange={e =>
            onChange(
              Event.Slots.Scheme.Function(
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
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-explicit"> {React.string("Explicit")} </Label>
        <input
          name="inspector-sch-explicit"
          type_="checkbox"
          checked={slots.explicit}
          onChange={e =>
            onChange(Event.Slots.Scheme.Explicit(ReactEvent.Form.target(e)["checked"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-scope"> {React.string("Scope")} </Label>
        <select
          name="inspector-sch-scope"
          value={Scope.toString(slots.scope)}
          onChange={e =>
            onChange(
              Event.Slots.Scheme.Scope(
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
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-organisation"> {React.string("Organisation")} </Label>
        <Input
          value={slots.organisation}
          name="inspector-sch-organisation"
          onChange={e =>
            onChange(Event.Slots.Scheme.Organisation(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Notes
        name="inspector-sch-notes"
        onChange={e => onChange(Event.Slots.Scheme.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Dimension = {
  @react.component
  let make = (~slots: InspectorState.Dimension.t, ~onChange) => {
    <>
      <Row>
        <Label htmlFor="inspector-dim-concept"> {React.string("Concept")} </Label>
        <Input
          value={slots.concept}
          name="inspector-dim-concept"
          onChange={e =>
            onChange(Event.Slots.Dimension.Concept(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-concept-scale"> {React.string("Concept Scale")} </Label>
        <select
          name="inspector-dim-concept-scale"
          value={Quantity_scale.toString(slots.concept_scale)}
          onChange={e =>
            onChange(
              Event.Slots.Dimension.Concept_scale(
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
      </Row>
      // Need to add concept attributes
      <Row>
        <Label htmlFor="inspector-dim-graphic"> {React.string("Graphic")} </Label>
        <Input
          value={slots.graphic}
          name="inspector-dim-graphic"
          onChange={e =>
            onChange(Event.Slots.Dimension.Graphic(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-graphic-scale"> {React.string("Graphic Scale")} </Label>
        <select
          name="inspector-dim-graphic-scale"
          value={Quantity_scale.toString(slots.graphic_scale)}
          onChange={e =>
            onChange(
              Event.Slots.Dimension.Graphic_scale(
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
      </Row>
      // Need to add graphic attributes
      <Row>
        <Label htmlFor="inspector-dim-function"> {React.string("Function")} </Label>
        <select
          name="inspector-dim-function"
          value={Function.toString(slots.function)}
          onChange={e =>
            onChange(
              Event.Slots.Dimension.Function(
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
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-explicit"> {React.string("Explicit")} </Label>
        <input
          name="inspector-dim-explicit"
          type_="checkbox"
          checked={slots.explicit}
          onChange={e =>
            onChange(Event.Slots.Dimension.Explicit(ReactEvent.Form.target(e)["checked"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-scope"> {React.string("Scope")} </Label>
        <select
          name="inspector-dim-scope"
          value={Scope.toString(slots.scope)}
          onChange={e =>
            onChange(
              Event.Slots.Dimension.Scope(
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
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-organisation"> {React.string("Organisation")} </Label>
        <Input
          value={slots.organisation}
          name="inspector-dim-organisation"
          onChange={e =>
            onChange(Event.Slots.Dimension.Organisation(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Notes
        name="inspector-dim-notes"
        onChange={e => onChange(Event.Slots.Dimension.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Token = {
  @react.component
  let make = (~slots: InspectorState.Token.t, ~onChange) => {
    <>
      <Row>
        <Label htmlFor="inspector-tok-concept"> {React.string("Concept")} </Label>
        <Input
          value={slots.concept}
          name="inspector-tok-concept"
          onChange={e => onChange(Event.Slots.Token.Concept(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-tok-graphic"> {React.string("Graphic")} </Label>
        <Input
          value={slots.graphic}
          name="inspector-tok-graphic"
          onChange={e => onChange(Event.Slots.Token.Graphic(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-tok-class"> {React.string("Is class")} </Label>
        <input
          name="inspector-tok-class"
          type_="checkbox"
          checked={slots.is_class}
          onChange={e => onChange(Event.Slots.Token.Is_class(ReactEvent.Form.target(e)["checked"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-tok-function"> {React.string("Function")} </Label>
        <select
          name="inspector-tok-function"
          value={Function.toString(slots.function)}
          onChange={e =>
            onChange(
              Event.Slots.Token.Function(
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
      </Row>
      <Row>
        <Label htmlFor="inspector-tok-explicit"> {React.string("Explicit")} </Label>
        <input
          name="inspector-tok-explicit"
          type_="checkbox"
          checked={slots.explicit}
          onChange={e => onChange(Event.Slots.Token.Explicit(ReactEvent.Form.target(e)["checked"]))}
        />
      </Row>
      <Notes
        name="inspector-tok-notes"
        onChange={e => onChange(Event.Slots.Token.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Placeholder = {
  @react.component
  let make = (~slots: InspectorState.Placeholder.t, ~onChange) => {
    <>
      <Row>
        <Label htmlFor="inspector-placeholder-description"> {React.string("Description")} </Label>
        <Input
          value={slots.description}
          name="inspector-placeholder-description"
          onChange={e =>
            onChange(Event.Slots.Placeholder.Description(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-placeholder-intensional">
          {React.string("Omitted but understood?")}
        </Label>
        <input
          name="inspector-placeholder-intensional"
          type_="checkbox"
          checked={slots.isIntensional}
          onChange={e =>
            onChange(Event.Slots.Placeholder.IsIntensional(ReactEvent.Form.target(e)["checked"]))}
        />
      </Row>
      <Notes
        name="inspector-placeholder-description"
        onChange={e => onChange(Event.Slots.Placeholder.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Model = {
  @react.component
  let make = (~slots: InspectorState.Model.t, ~onChange) => {
    <>
      <Row>
        <Label htmlFor="inspector-model-name"> {React.string("Name")} </Label>
        <Input
          value={slots.name}
          name="inspector-model-name"
          onChange={e => onChange(Event.Model.Rename(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Notes
        name="inspector-model-notes"
        onChange={e => onChange(Event.Model.SetNotes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

@react.component
let make = (~id, ~data, ~onChange=?) => {
  let onChange = onChange->Option.getWithDefault(_ => ())
  <HideablePanel
    id
    className="inspector-panel"
    toggle={(~hidden) =>
      <div
        style={ReactDOM.Style.make(
          ~cursor="default",
          ~userSelect="none",
          ~position="absolute",
          ~bottom="5px",
          ~right={
            if hidden {
              "10px"
            } else {
              "350px"
            }
          },
          ~fontSize="16px",
          (),
        )}>
        {if hidden {
          React.string(Js.String2.fromCharCode(9001))
        } else {
          React.string(Js.String2.fromCharCode(9002))
        }}
      </div>}
    style={ReactDOM.Style.make(
      ~order="2",
      ~padding="0.5rem 0",
      ~width="350px",
      ~display="flex",
      ~flexDirection="column",
      ~borderLeft="1px solid black",
      (),
    )}>
    {switch data {
    | InspectorState.Empty =>
      <span
        style={ReactDOM.Style.make(
          ~display="block",
          ~marginTop="50%",
          ~color="grey",
          ~fontSize="small",
          ~textAlign="center",
          (),
        )}
        className="inspector-panel-empty-message">
        {React.string("Select a model")}
      </span>
    | InspectorState.Global(slots) => <Model slots onChange />
    | InspectorState.Multiple(_) =>
      <span
        style={ReactDOM.Style.make(
          ~display="block",
          ~marginTop="50%",
          ~color="grey",
          ~fontSize="small",
          ~textAlign="center",
          (),
        )}
        className="inspector-panel-multiple-message">
        {React.string("Multiple schema selected")}
      </span>
    | InspectorState.Single(nodeId, schema) =>
      switch schema {
      | InspectorState.Schema.Representation(slots) =>
        <Representation
          slots onChange={c => onChange(Event.Model.Slots(nodeId, Event.Slots.Representation(c)))}
        />
      | InspectorState.Schema.Scheme(slots) =>
        <Scheme slots onChange={c => onChange(Event.Model.Slots(nodeId, Event.Slots.Scheme(c)))} />
      | InspectorState.Schema.Dimension(slots) =>
        <Dimension
          slots onChange={c => onChange(Event.Model.Slots(nodeId, Event.Slots.Dimension(c)))}
        />
      | InspectorState.Schema.Token(slots) =>
        <Token slots onChange={c => onChange(Event.Model.Slots(nodeId, Event.Slots.Token(c)))} />
      | InspectorState.Schema.Placeholder(slots) =>
        <Placeholder
          slots onChange={c => onChange(Event.Model.Slots(nodeId, Event.Slots.Placeholder(c)))}
        />
      }
    }}
  </HideablePanel>
}
