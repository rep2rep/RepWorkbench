module type Make_S = (Token : Schema_intf.S,
                      Dimension : Schema_intf.S,
                      Scheme : Schema_intf.S) => {
    type rec t = {
      domain : string,
      display : Graphic.t,
      tokens : list<Token.t>,
      dimensions : list<Dimension.t>,
      schemes : list<Scheme.t>,
      subrepresentations : list<t>
    }

    let validate : t => bool
  }

module Make = (Token : Schema_intf.S,
               Dimension : Schema_intf.S,
               Scheme : Schema_intf.S)
  => {
  type rec t = {
    domain : string,
    display : Graphic.t,
    tokens : list<Token.t>,
    dimensions : list<Dimension.t>,
    schemes : list<Scheme.t>,
    subrepresentations : list<t>
  }

  let validate = (t) => {
    List.length(t.tokens) > 0 ||
      List.length(t.dimensions) > 0 ||
      List.length(t.schemes) > 0
  }
}
