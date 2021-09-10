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

  let rec toJson = t =>
    Js.Dict.fromList(list{
      ("domain", Js.Json.string(t.domain)),
      ("display", Graphic.toJson(t.display)),
      ("tokens", t.tokens->List.toJson(Token.toJson)),
      ("dimensions", t.dimensions->List.toJson(Dimension.toJson)),
      ("schemes", t.schemes->List.toJson(Scheme.toJson)),
      ("subrepresentations", t.subrepresentations->List.toJson(toJson)),
    })->Js.Json.object_

  let rec fromJson = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let domain = get_value("domain", Js.Json.decodeString)
      let display = get_value("display", Graphic.fromJson)
      let tokens = get_value("tokens", j => j->List.fromJson(Token.fromJson))
      let dimensions = get_value("dimensions", j => j->List.fromJson(Dimension.fromJson))
      let schemes = get_value("schemes", j => j->List.fromJson(Scheme.fromJson))
      let subrepresentations = get_value("subrepresentations", j => j->List.fromJson(fromJson))
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
