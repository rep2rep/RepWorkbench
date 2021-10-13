@@warning("-30")

type rec representation = {
  uuid: Uuid.t,
  domain: string,
  display: Graphic.t,
  tokens: list<token>,
  dimensions: list<dimension>,
  schemes: list<scheme>,
  subrepresentations: list<representation>,
}
and scheme = {
  uuid: Uuid.t,
  concept_structure: string,
  graphic_structure: option<Graphic.t>,
  function: Function.t,
  explicit: bool,
  scope: Scope.t,
  tokens: list<token>,
  dimensions: Non_empty_list.t<dimension>,
  schemes: list<scheme>,
  organisation: string,
}
and dimension = {
  uuid: Uuid.t,
  concept: string,
  concept_scale: Quantity_scale.t,
  concept_attributes: list<Concept_attribute.t>,
  graphic: option<Graphic.t>,
  graphic_scale: Quantity_scale.t,
  graphic_attributes: list<Graphic_attribute.t>,
  function: Function.t,
  scope: Scope.t,
  explicit: bool,
  dimensions: list<dimension>,
  tokens: Non_empty_list.t<token>,
  organisation: string,
}
and token = {
  uuid: Uuid.t,
  concept: string,
  graphic: option<Graphic.t>,
  is_class: bool,
  function: Function.t,
  explicit: bool,
  sub_tokens: list<token>,
  anchored_tokens: list<token>,
  anchored_dimensions: list<dimension>,
  anchored_schemes: list<scheme>,
}

type schema =
  | Representation
  | Scheme
  | Dimension
  | Token

type fromJsonHelper = (
  Js.Dict.t<Js.Json.t>,
  Uuid.t,
  Uuid.Map.t<representation>,
  Uuid.Map.t<scheme>,
  Uuid.Map.t<dimension>,
  Uuid.Map.t<token>,
) => Or_error.t<(
  Uuid.Map.t<representation>,
  Uuid.Map.t<scheme>,
  Uuid.Map.t<dimension>,
  Uuid.Map.t<token>,
)>

module type S = {
  type t
  let uuid: t => Uuid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>

  let _toJsonHelper: (t, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t)
  let _fromJsonHelper: fromJsonHelper
}

let collapse: (
  list<'a>,
  list<(Uuid.t, Js.Json.t)>,
  Uuid.Set.t,
  'a => Uuid.t,
  ('a, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t),
) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t) = (lst, jsons, idxs, getUuid, toJsonHelper) =>
  List.reduce(lst, (jsons, idxs), ((json, ids), t') => {
    if Uuid.Set.has(ids, getUuid(t')) {
      (json, ids)
    } else {
      let (json', ids') = toJsonHelper(t', ids)
      (List.concat(json, json'), Uuid.Set.union(ids, ids'))
    }
  })

let recurse: (
  Or_error.t<list<Uuid.t>>,
  (
    Js.Dict.t<Js.Json.t>,
    Uuid.t,
    Uuid.Map.t<'rep>,
    Uuid.Map.t<'sch>,
    Uuid.Map.t<'dim>,
    Uuid.Map.t<'tok>,
  ) => Or_error.t<(Uuid.Map.t<'rep>, Uuid.Map.t<'sch>, Uuid.Map.t<'dim>, Uuid.Map.t<'tok>)>,
  Js.Dict.t<Js.Json.t>,
  schema,
  Uuid.Map.t<'rep>,
  Uuid.Map.t<'sch>,
  Uuid.Map.t<'dim>,
  Uuid.Map.t<'tok>,
) => Or_error.t<(Uuid.Map.t<'rep>, Uuid.Map.t<'sch>, Uuid.Map.t<'dim>, Uuid.Map.t<'tok>)> = (
  lst,
  fromJsonHelper,
  global_dict,
  schema,
  representations,
  schemes,
  dimensions,
  tokens,
) =>
  lst->Or_error.flatMap(lst =>
    lst->List.reduce(Or_error.create((representations, schemes, dimensions, tokens)), (
      maps,
      uuid,
    ) => {
      let keep_fst = (_, v1, v2) => {
        switch (v1, v2) {
        | (None, None) => None
        | (None, Some(v)) => Some(v)
        | (Some(v), _) => Some(v)
        }
      }
      maps->Or_error.flatMap(((representations, schemes, dimensions, tokens)) => {
        let alreadyExists = switch schema {
        | Representation => representations->Uuid.Map.has(uuid)
        | Scheme => schemes->Uuid.Map.has(uuid)
        | Dimension => dimensions->Uuid.Map.has(uuid)
        | Token => tokens->Uuid.Map.has(uuid)
        }
        if alreadyExists {
          Or_error.create((representations, schemes, dimensions, tokens))
        } else {
          global_dict
          ->fromJsonHelper(uuid, representations, schemes, dimensions, tokens)
          ->Or_error.map(((representations', schemes', dimensions', tokens')) => (
            Uuid.Map.merge(representations, representations', keep_fst),
            Uuid.Map.merge(schemes, schemes', keep_fst),
            Uuid.Map.merge(dimensions, dimensions', keep_fst),
            Uuid.Map.merge(tokens, tokens', keep_fst),
          ))
        }
      })
    })
  )
