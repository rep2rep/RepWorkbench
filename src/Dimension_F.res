module Make = (Token : Schema_intf.S) => {
  type rec t = {
    concept : string,
    concept_scale : Quantity_scale.t,
    concept_attributes : list<Concept_attribute.t>,
    graphic : option<Graphic.t>,
    graphic_scale : Quantity_scale.t,
    graphic_attributes : list<Graphic_attribute.t>,
    function : Function.t,
    scope : Scope.t,
    explicit : bool,
    dimensions : list<t>,
    tokens : Non_empty_list.t<Token.t>
  }

  let validate = (t) =>
      t.explicit === !Option.isNone(t.graphic)

}
