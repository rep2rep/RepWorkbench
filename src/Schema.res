module rec Representation : {
  type rec t = {
    domain : string,
    display : Graphic.t,
    tokens : list<Token.t>,
    dimensions : list<Dimension.t>,
    schemes : list<Scheme.t>,
    subrepresentations : list<t>
  }

  let validate : t => bool
} = Representation_F.Make(Token, Dimension, Scheme)

and Dimension : {
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

  let validate : (t) => bool
} = Dimension_F.Make(Token)

and Scheme : {
  type rec t = {
    concept_structure : string,
    graphic_structure : option<Graphic.t>,
    function : Function.t,
    explicit : bool,
    scope : Scope.t,
    tokens : list<Token.t>,
    dimensions : list<Dimension.t>,
    schemes : list<t>,
    organisation : string
  }

  let validate : (t) => bool
} = Scheme_F.Make(Dimension, Token)

and Token : {
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
} = Token_F.Make(Dimension, Scheme)

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let validate = (t) => {
  switch t {
      | Representation(r) => Representation.validate(r)
      | Scheme(s) => Scheme.validate(s)
      | Dimension(d) => Dimension.validate(d)
      | Token(t) => Token.validate(t)
  }
}
