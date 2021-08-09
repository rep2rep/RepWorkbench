module type Make_S = (Dimension : Schema_intf.S,
                      Scheme : Schema_intf.S) => {
  type rec t = {
    concept : string,
    graphic : option<Graphic.t>,
    function : Function.t,
    explicit : bool,
    tokens : list<t>,
    dimensions : list<Dimension.t>,
    schemes : list<Scheme.t>
  }

  let validate : (t) => bool
}

module Make : Make_S = (Dimension : Schema_intf.S,
                        Scheme : Schema_intf.S) => {
  type rec t = {
    concept : string,
    graphic : option<Graphic.t>,
    function : Function.t,
    explicit : bool,
    tokens : list<t>,
    dimensions : list<Dimension.t>,
    schemes : list<Scheme.t>
  }

  let validate = (t) =>
      t.explicit === !Belt.Option.isNone(t.graphic)
}
