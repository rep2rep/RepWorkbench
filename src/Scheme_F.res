module Make = (Dimension: Schema_intf.S, Token: Schema_intf.S) => {
  type rec t = {
    concept_structure: string,
    graphic_structure: option<Graphic.t>,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Token.t>,
    dimensions: list<Dimension.t>,
    schemes: list<t>,
    organisation: string,
  }

  let validate = t => t.explicit === !Option.isNone(t.graphic_structure)
}
