@@warning("-30")

type rec representation = {
  id: Gid.t,
  domain: string,
  display: Graphic.t,
  tokens: list<token>,
  dimensions: list<dimension>,
  schemes: list<scheme>,
  subrepresentations: list<representation>,
}
and scheme = {
  id: Gid.t,
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
  id: Gid.t,
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
  id: Gid.t,
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
  Gid.t,
  Gid.Map.t<representation>,
  Gid.Map.t<scheme>,
  Gid.Map.t<dimension>,
  Gid.Map.t<token>,
) => Or_error.t<(
  Gid.Map.t<representation>,
  Gid.Map.t<scheme>,
  Gid.Map.t<dimension>,
  Gid.Map.t<token>,
)>

module type S = {
  type t
  let id: t => Gid.t
  let validate: t => Or_error.t<unit>

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>

  let _toJsonHelper: (t, Gid.Set.t) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t)
  let _fromJsonHelper: fromJsonHelper
}

let collapse: (
  list<'a>,
  list<(Gid.t, Js.Json.t)>,
  Gid.Set.t,
  'a => Gid.t,
  ('a, Gid.Set.t) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t),
) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t) = (lst, jsons, idxs, getGid, toJsonHelper) =>
  List.reduce(lst, (jsons, idxs), ((json, ids), t') => {
    if Gid.Set.has(ids, getGid(t')) {
      (json, ids)
    } else {
      let (json', ids') = toJsonHelper(t', ids)
      (List.concat(json, json'), Gid.Set.union(ids, ids'))
    }
  })

let recurse: (
  Or_error.t<list<Gid.t>>,
  (
    Js.Dict.t<Js.Json.t>,
    Gid.t,
    Gid.Map.t<'rep>,
    Gid.Map.t<'sch>,
    Gid.Map.t<'dim>,
    Gid.Map.t<'tok>,
  ) => Or_error.t<(Gid.Map.t<'rep>, Gid.Map.t<'sch>, Gid.Map.t<'dim>, Gid.Map.t<'tok>)>,
  Js.Dict.t<Js.Json.t>,
  schema,
  Gid.Map.t<'rep>,
  Gid.Map.t<'sch>,
  Gid.Map.t<'dim>,
  Gid.Map.t<'tok>,
) => Or_error.t<(Gid.Map.t<'rep>, Gid.Map.t<'sch>, Gid.Map.t<'dim>, Gid.Map.t<'tok>)> = (
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
        | Representation => representations->Gid.Map.has(uuid)
        | Scheme => schemes->Gid.Map.has(uuid)
        | Dimension => dimensions->Gid.Map.has(uuid)
        | Token => tokens->Gid.Map.has(uuid)
        }
        if alreadyExists {
          Or_error.create((representations, schemes, dimensions, tokens))
        } else {
          global_dict
          ->fromJsonHelper(uuid, representations, schemes, dimensions, tokens)
          ->Or_error.map(((representations', schemes', dimensions', tokens')) => (
            Gid.Map.merge(representations, representations', keep_fst),
            Gid.Map.merge(schemes, schemes', keep_fst),
            Gid.Map.merge(dimensions, dimensions', keep_fst),
            Gid.Map.merge(tokens, tokens', keep_fst),
          ))
        }
      })
    })
  )
