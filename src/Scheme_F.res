module Make = (Dimension: Schema_intf.S, Token: Schema_intf.S) => {
  type rec t = {
    concept_structure: string,
    concept_type: string,
    graphic_structure: option<Graphic.t>,
    graphic_type: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Token.t>,
    dimensions: Non_empty_list.t<Dimension.t>,
    schemes: list<t>,
    organisation: string,
  }

  let rec validate = t =>
    t.explicit === !Option.isNone(t.graphic_structure) &&
    t.tokens->List.every(Token.validate) &&
    t.dimensions->Non_empty_list.every(Dimension.validate) &&
    t.schemes->List.every(validate)
}
