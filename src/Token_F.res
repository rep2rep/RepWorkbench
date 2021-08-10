module Make = (Dimension: Schema_intf.S, Scheme: Schema_intf.S) => {
  type rec t = {
    concept: string,
    graphic: option<Graphic.t>,
    function: Function.t,
    explicit: bool,
    tokens: list<t>,
    dimensions: list<Dimension.t>,
    schemes: list<Scheme.t>,
  }

  let validate = t => t.explicit === !Option.isNone(t.graphic)
}
