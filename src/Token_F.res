module Make = (Dimension: Schema_intf.S, Scheme: Schema_intf.S) => {
  module Level = {
    type t = Atomic | Expression
  }

  type rec t = {
    concept: string,
    concept_type: string,
    graphic: option<Graphic.t>,
    graphic_type: string,
    level: Level.t,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Dimension.t>,
    anchored_schemes: list<Scheme.t>,
  }

  let rec validate = t =>
    t.explicit === !Option.isNone(t.graphic) &&
    t.sub_tokens->List.every(validate) &&
    t.anchored_tokens->List.every(validate) &&
    t.anchored_dimensions->List.every(Dimension.validate) &&
    t.anchored_schemes->List.every(Scheme.validate)
}
