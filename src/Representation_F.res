module Make = (Token: Schema_intf.S, Dimension: Schema_intf.S, Scheme: Schema_intf.S) => {
  type rec t = {
    domain: string,
    display: Graphic.t,
    tokens: list<Token.t>,
    dimensions: list<Dimension.t>,
    schemes: list<Scheme.t>,
    subrepresentations: list<t>,
  }

  let rec validate = t => {
    (List.length(t.tokens) > 0 || List.length(t.dimensions) > 0 || List.length(t.schemes) > 0) &&
    t.tokens->List.every(Token.validate) &&
    t.dimensions->List.every(Dimension.validate) &&
    t.schemes->List.every(Scheme.validate) &&
    t.subrepresentations->List.every(validate)
  }
}
