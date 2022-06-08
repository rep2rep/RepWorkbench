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
  let make = (~style=ReactDOM.Style.make(), ~children) => {
    <div
      style={ReactDOM.Style.make(
        ~margin="0.125rem 0.5rem",
        ~minHeight="20px",
        ~display="flex",
        ~alignItems="center",
        (),
      )->ReactDOM.Style.combine(style)}
      className="inspector-row">
      {children}
    </div>
  }
}

module Title = {
  @react.component
  let make = (~value: string) => {
    <Row>
      <h3 style={ReactDOM.Style.make(~marginBottom="0.5ex", ())}> {React.string(value)} </h3>
    </Row>
  }
}

module Label = {
  @react.component
  let make = (~htmlFor=?, ~help as title=?, ~style=ReactDOM.Style.make(), ~children) => {
    <label
      style={ReactDOM.Style.make(
        ~fontSize="small",
        ~marginRight="0.5rem",
        (),
      )->ReactDOM.Style.combine(style)}
      ?htmlFor
      ?title>
      {children}
    </label>
  }
}

module Input = {
  @react.component
  let make = (~name=?, ~value=?, ~onChange=?, ~style=ReactDOM.Style.make()) => {
    <input
      type_="text"
      ?name
      ?value
      ?onChange
      style={ReactDOM.Style.make(
        ~flexGrow="1",
        ~border="1px solid #777",
        ~borderRadius="2px",
        ~padding="0.125rem 0.25rem",
        (),
      )->ReactDOM.Style.combine(style)}
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

module AttributesEditor = {
  @react.component
  let make = (~name, ~value, ~onChange) => {
    let onChange = e => {
      Js.Console.log(e)
      onChange(e)
    }
    let add = _ => value->List.concat(List.singleton(""))->onChange
    let edit = (e, i) =>
      value
      ->List.mapWithIndex((idx, v) =>
        if idx === i {
          ReactEvent.Form.target(e)["value"]
        } else {
          v
        }
      )
      ->onChange
    let remove = i => value->List.keepWithIndex((_, idx) => idx !== i)->onChange
    <div style={ReactDOM.Style.make(~display="inline-block", ())}>
      {value
      ->List.toArray
      ->Array.mapWithIndex((idx, attr) => {
        <span
          key={name ++ "-" ++ Int.toString(idx)} style={ReactDOM.Style.make(~display="block", ())}>
          <Input
            value={attr}
            onChange={e => edit(e, idx)}
            style={ReactDOM.Style.make(~marginBottom="0.25rem", ~marginRight="0.5rem", ())}
          />
          <input type_="button" value="Delete" onClick={_ => remove(idx)} />
        </span>
      })
      ->React.array}
      <input type_="button" value="Add Attribute" onClick=add />
    </div>
  }
}

module Notes = {
  @react.component
  let make = (~name, ~value=?, ~onChange=?, ~help=?) => {
    let help = help->Option.getWithDefault("Add any other comments about this schema here.")
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexDirection="column",
        ~margin="0.125rem 0.5rem",
        (),
      )}>
      <Label htmlFor={name} help> {React.string("Notes")} </Label>
      <textarea
        name
        ?onChange
        ?value
        style={ReactDOM.Style.make(
          ~height="200px",
          ~border="1px solid #777",
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
      <Title value="Representation" />
      <Row>
        <Label htmlFor="inspector-rep-domain" help="Indicate the title of the diagram.">
          {React.string("Domain")}
        </Label>
        <Input
          value={slots.domain}
          name="inspector-rep-domain"
          onChange={e =>
            onChange(Event.Slots.Representation.Domain(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-rep-display"
          help="Indicate the physical location of the diagram (e.g., top right image, Dropbox folder).">
          {React.string("Display")}
        </Label>
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
      <Title value="R-Scheme" />
      <Row>
        <Label htmlFor="inspector-sch-concept" help="A description of the concept.">
          {React.string("Concept")}
        </Label>
        <Input
          value={slots.concept_structure}
          name="inspector-sch-concept"
          onChange={e =>
            onChange(Event.Slots.Scheme.Concept_structure(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-sch-graphic"
          help="The annotation used in the diagram that represents the concept. If the concept is represented then use ## to indicate that there is no graphic element for the concept.">
          {React.string("Graphic")}
        </Label>
        <Input
          value={slots.graphic_structure}
          name="inspector-sch-graphic"
          onChange={e =>
            onChange(Event.Slots.Scheme.Graphic_structure(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-sch-function"
          help="Specifies the role of the schema as semantic, auxiliary, or arbitrary. Select semantic if the schema is essential for the interpretation; auxiliary if the schema is to pragmatically aid the interpretation; and arbitrary if it only has a decoratively or aesthetic purpose.">
          {React.string("Function")}
        </Label>
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
        <Label
          htmlFor="inspector-sch-explicit"
          help="Yes, if there is a graphic object for the concept. No, if there is not a graphic object for the concept.">
          {React.string("Explicit?")}
        </Label>
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
        <Label
          htmlFor="inspector-sch-scope"
          help="Global if it affects the overall representation. Local, if it affects part of the representation.">
          {React.string("Scope")}
        </Label>
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
        <Label
          htmlFor="inspector-sch-organisation"
          help="Indicated how the concept is related to the graphic. Summarises how all the pieces in the R-Scheme schema are put together.">
          {React.string("Organisation")}
        </Label>
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
      <Title value="R-Dimension" />
      <Row>
        <Label htmlFor="inspector-dim-concept" help="A description of the concept.">
          {React.string("Concept")}
        </Label>
        <Input
          value={slots.concept}
          name="inspector-dim-concept"
          onChange={e =>
            onChange(Event.Slots.Dimension.Concept(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-dim-concept-scale"
          help="Indicates what kind of quantity is associated with the concept. If the concept is about categories or groups, use Nominal; for concepts that can be ordered, use Ordinal; if instances in the concept can be ordered at specific intervals, use Interval; and for instances of the concept whose proportions can be compared, use Ratio.">
          {React.string("Concept Scale")}
        </Label>
        <Selector
          name="inspector-dim-concept-scale"
          options={Quantity_scale.all}
          current={slots.concept_scale}
          toString={Quantity_scale.toString}
          fromString={Quantity_scale.fromString}
          onChange={e => onChange(Event.Slots.Dimension.Concept_scale(e))}
        />
      </Row>
      <Row style={ReactDOM.Style.make(~alignItems="top", ())}>
        <Label
          htmlFor="inspector-dim-concept-attr"
          style={ReactDOM.Style.make(~marginTop="0.125rem", ())}
          help="List of attributes that are relevant to the concept. They may not be present in the graphic. Example: maximum values, minimum values.">
          {React.string("Concept Attributes")}
        </Label>
        <AttributesEditor
          name="inspector-dim-concept-attr"
          value={slots.concept_attributes}
          onChange={e => onChange(Event.Slots.Dimension.Concept_attributes(e))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-dim-graphic"
          help="The annotation used in the diagram that represents the concept. If the concept is represented then use ## to indicate that there is no graphic element for the concept.">
          {React.string("Graphic")}
        </Label>
        <Input
          value={slots.graphic}
          name="inspector-dim-graphic"
          onChange={e =>
            onChange(Event.Slots.Dimension.Graphic(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-dim-graphic-scale"
          help="Indicates what kind of quantity is associated with the graphic. If the graphic can be categorised or grouped, use Nominal; if the graphic that can be ordered, use Ordinal; if instances in the graphic can be ordered at specific intervals, use Interval; and for instances of the graphic whose proportions can be compared, use Ratio.">
          {React.string("Graphic Scale")}
        </Label>
        <Selector
          name="inspector-dim-graphic-scale"
          options={Quantity_scale.all}
          current={slots.graphic_scale}
          toString={Quantity_scale.toString}
          fromString={Quantity_scale.fromString}
          onChange={e => onChange(Event.Slots.Dimension.Graphic_scale(e))}
        />
      </Row>
      <Row style={ReactDOM.Style.make(~alignItems="top", ())}>
        <Label
          htmlFor="inspector-dim-graphic-attr"
          style={ReactDOM.Style.make(~marginTop="0.125rem", ())}
          help="Parts of the display that can potentially be meaningful. Example: blue, red.">
          {React.string("Graphic Attributes")}
        </Label>
        <AttributesEditor
          name="inspector-dim-graphic-attr"
          value={slots.graphic_attributes}
          onChange={e => onChange(Event.Slots.Dimension.Graphic_attributes(e))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-dim-function"
          help="Specifies the role of the schema as semantic, auxiliary, or arbitrary. Select semantic if the schema is essential for the interpretation; auxiliary if the schema is to pragmatically aid the interpretation; and arbitrary if it only has a decoratively or aesthetic purpose.">
          {React.string("Function")}
        </Label>
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
        <Label
          htmlFor="inspector-dim-explicit"
          help="Yes, if there is a graphic object for the concept. No, if there is not a graphic object for the concept.">
          {React.string("Explicit?")}
        </Label>
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
        <Label
          htmlFor="inspector-dim-scope"
          help="Global if it affects the overall representation. Local, if it affects part of the representation.">
          {React.string("Scope")}
        </Label>
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
        <Label
          htmlFor="inspector-dim-organisation"
          help="Indicated how the concept is related to the graphic. Summarises how all the pieces in the R-Dimension schema are put together.">
          {React.string("Organisation")}
        </Label>
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
      <Title value="R-Symbol" />
      <Row>
        <Label htmlFor="inspector-tok-concept" help="A description of the concept.">
          {React.string("Concept")}
        </Label>
        <Input
          value={slots.concept}
          name="inspector-tok-concept"
          onChange={e => onChange(Event.Slots.Token.Concept(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-tok-graphic"
          help="The annotation used in the diagram that represents the concept. If the concept is  not represented, use '##' to indicate that there is no graphic element for the concept.">
          {React.string("Graphic")}
        </Label>
        <Input
          value={slots.graphic}
          name="inspector-tok-graphic"
          onChange={e => onChange(Event.Slots.Token.Graphic(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-tok-class"
          help="Yes, if this R-symbol is standing in for many closely related concepts. No, if this R-symbol is just for one concept.">
          {React.string("Is class?")}
        </Label>
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
        <Label
          htmlFor="inspector-tok-function"
          help="Specifies the role of the schema as semantic, auxiliary, or arbitrary. Select semantic if the schema is essential for the interpretation; auxiliary if the schema is to pragmatically aid the interpretation; and arbitrary if it only has a decoratively or aesthetic purpose.">
          {React.string("Function")}
        </Label>
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
        <Label
          htmlFor="inspector-tok-explicit"
          help="Yes, if there is a graphic object for the concept. No, if there is not a graphic object for the concept.">
          {React.string("Explicit?")}
        </Label>
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
      <Title value="Placeholder" />
      <Row>
        <Label
          htmlFor="inspector-placeholder-description"
          help="Briefly describe what you are omitting.">
          {React.string("Description")}
        </Label>
        <Input
          value={slots.description}
          name="inspector-placeholder-description"
          onChange={e =>
            onChange(Event.Slots.Placeholder.Description(ReactEvent.Form.target(e)["value"]))}
        />
      </Row>
      <Row>
        <Label
          htmlFor="inspector-placeholder-intensional"
          help="If you are inserting this node because this interpretation is missing aspects that would go in the model, select \"No\". If instead this aspect is being omitted purely for space/time/effort reasons, but the interpretation is complete, select \"Yes\".">
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
        name="inspector-placeholder-notes"
        onChange={e => onChange(Event.Slots.Placeholder.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Hierarchy = {
  @react.component
  let make = (~slots: InspectorState.Hierarchy.t, ~onChange) => {
    <>
      <Title value="Hierarchy" />
      <Notes
        name="inspector-hierarchy-notes"
        onChange={e => onChange(Event.Slots.Hierarchy.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Anchor = {
  @react.component
  let make = (~slots: InspectorState.Anchor.t, ~onChange) => {
    <>
      <Title value="Anchor" />
      <Notes
        name="inspector-anchor-notes"
        onChange={e => onChange(Event.Slots.Anchor.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Relation = {
  @react.component
  let make = (~slots: InspectorState.Relation.t, ~onChange) => {
    <>
      <Title value="Equivalence" />
      <Notes
        name="inspector-relation-notes"
        onChange={e => onChange(Event.Slots.Relation.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Overlap = {
  @react.component
  let make = (~slots: InspectorState.Overlap.t, ~onChange) => {
    <>
      <Title value="Overlap" />
      <Notes
        name="inspector-overlap-notes"
        onChange={e => onChange(Event.Slots.Overlap.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Disjoint = {
  @react.component
  let make = (~slots: InspectorState.Disjoint.t, ~onChange) => {
    <>
      <Title value="Disjoint" />
      <Notes
        name="inspector-disjoint-notes"
        onChange={e => onChange(Event.Slots.Disjoint.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Generic = {
  @react.component
  let make = (~slots: InspectorState.Generic.t, ~onChange) => {
    <>
      <Title value="Generic Link" />
      <Row> {React.string("This link is intended for internal use only.")} </Row>
      <Notes
        name="inspector-generic-notes"
        onChange={e => onChange(Event.Slots.Generic.Notes(ReactEvent.Form.target(e)["value"]))}
        value={slots.notes}
      />
    </>
  }
}

module Model = {
  @react.component
  let make = (~slots: InspectorState.Model.t, ~onChange) => {
    <>
      <Title value="Model" />
      <Row>
        <Label htmlFor="inspector-model-name" help="Set the name of this model.">
          {React.string("Name")}
        </Label>
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
        help="Add any other comments about this model here."
      />
    </>
  }
}

module SubInspector = {
  @react.component
  let make = (~children) => {
    let (visible, setVisible) = React.useState(_ => true)
    let style = ReactDOM.Style.make(
      ~border="1px solid #888",
      ~borderRadius="5px",
      ~boxShadow="0px 1px 3px #bbb",
      ~margin="0.125rem 0.25rem 0.5rem 0.25rem",
      ~padding="0.5rem 0",
      ~position="relative",
      (),
    )
    let toggler =
      <span
        onClick={e => {
          e->ReactEvent.Mouse.preventDefault
          setVisible(Bool.not)
        }}
        style={ReactDOM.Style.make(
          ~position="absolute",
          ~top="3px",
          ~right="5px",
          ~cursor="default",
          ~userSelect="none",
          (),
        )}>
        {React.string(
          if visible {
            String.fromCodePoint(8722)
          } else {
            "+"
          },
        )}
      </span>
    if visible {
      <div style> {toggler} {children} </div>
    } else {
      <div style={ReactDOM.Style.combine(style, ReactDOM.Style.make(~padding="0.75rem 0", ()))}>
        {toggler}
      </div>
    }
  }
}

let showSchema = (nodeId, schema, onChange) =>
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

let showLink = (linkId, link, onChange) =>
  switch link {
  | InspectorState.Link.Hierarchy(slots) =>
    <Hierarchy
      slots onChange={c => onChange(Event.Model.Slots(linkId, Event.Slots.Hierarchy(c)))}
    />
  | InspectorState.Link.Anchor(slots) =>
    <Anchor slots onChange={c => onChange(Event.Model.Slots(linkId, Event.Slots.Anchor(c)))} />
  | InspectorState.Link.Relation(slots) =>
    <Relation slots onChange={c => onChange(Event.Model.Slots(linkId, Event.Slots.Relation(c)))} />
  | InspectorState.Link.Overlap(slots) =>
    <Overlap slots onChange={c => onChange(Event.Model.Slots(linkId, Event.Slots.Overlap(c)))} />
  | InspectorState.Link.Disjoint(slots) =>
    <Disjoint slots onChange={c => onChange(Event.Model.Slots(linkId, Event.Slots.Disjoint(c)))} />
  | InspectorState.Link.Generic(slots) =>
    <Generic slots onChange={c => onChange(Event.Model.Slots(linkId, Event.Slots.Generic(c)))} />
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
      ~width="350px",
      ~display="flex",
      ~flexDirection="column",
      ~borderLeft="1px solid black",
      ~overflow="hidden",
      (),
    )}>
    <div
      style={ReactDOM.Style.make(
        ~display="flex",
        ~flexGrow="1",
        ~flexDirection="column",
        ~overflowY="auto",
        ~padding="0.5rem 0",
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
      | InspectorState.Multiple(allSlots) =>
        allSlots
        ->Array.map(((id, slots)) =>
          <SubInspector key={Gid.toString(id)}>
            {switch slots {
            | InspectorState.SchemaOrLink.Schema(schema) => showSchema(id, schema, onChange)
            | InspectorState.SchemaOrLink.Link(link) => showLink(id, link, onChange)
            }}
          </SubInspector>
        )
        ->React.array
      | InspectorState.Schema(nodeId, schema) => showSchema(nodeId, schema, onChange)
      | InspectorState.Link(linkId, link) => showLink(linkId, link, onChange)
      }}
    </div>
  </HideablePanel2>
}
