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

  let rec to_JSON = t =>
    Js.Dict.fromList(list{
      ("domain", Js.Json.string(t.domain)),
      ("display", Graphic.to_JSON(t.display)),
      ("tokens", t.tokens->List.to_JSON(Token.to_JSON)),
      ("dimensions", t.dimensions->List.to_JSON(Dimension.to_JSON)),
      ("schemes", t.schemes->List.to_JSON(Scheme.to_JSON)),
      ("subrepresentations", t.subrepresentations->List.to_JSON(to_JSON)),
    })->Js.Json.object_

  let rec of_JSON = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let domain = get_value("domain", Js.Json.decodeString)
      let display = get_value("display", Graphic.of_JSON)
      let tokens = get_value("tokens", j => j->List.of_JSON(Token.of_JSON))
      let dimensions = get_value("dimensions", j => j->List.of_JSON(Dimension.of_JSON))
      let schemes = get_value("schemes", j => j->List.of_JSON(Scheme.of_JSON))
      let subrepresentations = get_value("subrepresentations", j => j->List.of_JSON(of_JSON))
      switch (domain, display, tokens, dimensions, schemes, subrepresentations) {
      | (
          Some(domain),
          Some(display),
          Some(tokens),
          Some(dimensions),
          Some(schemes),
          Some(subrepresentations),
        ) =>
        Some({
          domain: domain,
          display: display,
          tokens: tokens,
          dimensions: dimensions,
          schemes: schemes,
          subrepresentations: subrepresentations,
        })
      | _ => None
      }
    })
}
