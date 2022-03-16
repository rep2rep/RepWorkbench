let boolToStringYN = b =>
  switch b {
  | true => "Yes"
  | false => "No"
  }
let boolFromStringYN = s =>
  switch s {
  | "Yes" => Some(true)
  | "No" => Some(false)
  | _ => None
  }

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
  let make = (~htmlFor=?, ~help as title=?, ~children) => {
    <label
      style={ReactDOM.Style.make(~fontSize="small", ~marginRight="0.5rem", ())} ?htmlFor ?title>
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

module Selector = {
  @react.component
  let make = (
    ~name: string,
    ~options: array<'a>,
    ~current: option<'a>,
    ~toString: 'a => string,
    ~fromString: string => option<'a>,
    ~onChange: option<'a> => unit,
  ) => {
    <select
      name
      value={current->Option.map(toString)->Option.getWithDefault("-")}
      onChange={e => onChange(fromString(ReactEvent.Form.target(e)["value"]))}>
      <option value="-" key={name ++ "-option-none"}> {React.string("-")} </option>
      {options
      ->Array.map(a =>
        <option value={toString(a)} key={name ++ "-option-" ++ toString(a)}>
          {React.string(toString(a))}
        </option>
      )
      ->React.array}
    </select>
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
        <Selector
          name="inspector-sch-function"
          options={Function.all}
          current={slots.function}
          toString={Function.toString}
          fromString={Function.fromString}
          onChange={f => onChange(Event.Slots.Scheme.Function(f))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-explicit"> {React.string("Explicit?")} </Label>
        <Selector
          name="inspector-sch-explicit"
          options={[true, false]}
          current={slots.explicit}
          toString={boolToStringYN}
          fromString={boolFromStringYN}
          onChange={e => onChange(Event.Slots.Scheme.Explicit(e))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-sch-scope"> {React.string("Scope")} </Label>
        <Selector
          name="inspector-sch-scope"
          options={Scope.all}
          current={slots.scope}
          toString={Scope.toString}
          fromString={Scope.fromString}
          onChange={e => onChange(Event.Slots.Scheme.Scope(e))}
        />
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
        <Selector
          name="inspector-dim-concept-scale"
          options={Quantity_scale.all}
          current={slots.concept_scale}
          toString={Quantity_scale.toString}
          fromString={Quantity_scale.fromString}
          onChange={e => onChange(Event.Slots.Dimension.Concept_scale(e))}
        />
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
        <Selector
          name="inspector-dim-graphic-scale"
          options={Quantity_scale.all}
          current={slots.graphic_scale}
          toString={Quantity_scale.toString}
          fromString={Quantity_scale.fromString}
          onChange={e => onChange(Event.Slots.Dimension.Graphic_scale(e))}
        />
      </Row>
      // Need to add graphic attributes
      <Row>
        <Label htmlFor="inspector-dim-function"> {React.string("Function")} </Label>
        <Selector
          name="inspector-dim-function"
          options={Function.all}
          current={slots.function}
          toString={Function.toString}
          fromString={Function.fromString}
          onChange={e => onChange(Event.Slots.Dimension.Function(e))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-explicit"> {React.string("Explicit?")} </Label>
        <Selector
          name="inspector-dim-explicit"
          options={[true, false]}
          current={slots.explicit}
          toString={boolToStringYN}
          fromString={boolFromStringYN}
          onChange={e => onChange(Event.Slots.Dimension.Explicit(e))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-dim-scope"> {React.string("Scope")} </Label>
        <Selector
          name="inspector-dim-scope"
          options={Scope.all}
          current={slots.scope}
          toString={Scope.toString}
          fromString={Scope.fromString}
          onChange={e => onChange(Event.Slots.Dimension.Scope(e))}
        />
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
        <Label htmlFor="inspector-tok-class"> {React.string("Is class?")} </Label>
        <Selector
          name="inspector-tok-class"
          options={[true, false]}
          current={slots.is_class}
          toString={boolToStringYN}
          fromString={boolFromStringYN}
          onChange={e => onChange(Event.Slots.Token.Is_class(e))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-tok-function"> {React.string("Function")} </Label>
        <Selector
          name="inspector-tok-function"
          options={Function.all}
          current={slots.function}
          toString={Function.toString}
          fromString={Function.fromString}
          onChange={e => onChange(Event.Slots.Token.Function(e))}
        />
      </Row>
      <Row>
        <Label htmlFor="inspector-tok-explicit"> {React.string("Explicit?")} </Label>
        <Selector
          name="inspector-tok-explicit"
          options={[true, false]}
          current={slots.explicit}
          toString={boolToStringYN}
          fromString={boolFromStringYN}
          onChange={e => onChange(Event.Slots.Token.Explicit(e))}
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
        <Selector
          name="inspector-placeholder-intensional"
          options={[true, false]}
          current={slots.isIntensional}
          toString={boolToStringYN}
          fromString={boolFromStringYN}
          onChange={e => onChange(Event.Slots.Placeholder.IsIntensional(e))}
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
  <HideablePanel2
    id
    className="inspector-panel"
    toggle={(~hidden) =>
      <div
        style={ReactDOM.Style.make(
          ~cursor="default",
          ~userSelect="none",
          ~position="absolute",
          ~top="40px",
          ~zIndex="100000",
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
  </HideablePanel2>
}
