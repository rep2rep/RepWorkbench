type rec t<'a> =
  | Singleton('a)
  | Cons('a, t<'a>)

let cons = (t, a) => Cons(a, t)

let singleton = a => Singleton(a)

let reverse = t => {
  let rec rev_helper = (t, t') => {
    switch t {
    | Singleton(x) => Cons(x, t')
    | Cons(x, xs) => rev_helper(xs, Cons(x, t'))
    }
  }
  switch t {
  | Singleton(x) => Singleton(x)
  | Cons(x, xs) => rev_helper(xs, Singleton(x))
  }
}

let rec fromList = xs =>
  switch xs {
  | list{} => None
  | list{x} => Some(Singleton(x))
  | list{x, ...rest} => Some(Cons(x, Option.getExn(fromList(rest))))
  }

let rec toList = t =>
  switch t {
  | Singleton(x) => list{x}
  | Cons(x, xs) => list{x, ...toList(xs)}
  }

let rec toArray = t =>
  switch t {
  | Singleton(x) => [x]
  | Cons(x, xs) => Array.concat([x], toArray(xs))
  }

let rec length = t =>
  switch t {
  | Singleton(_) => 1
  | Cons(_, xs) => 1 + length(xs)
  }

let head = t =>
  switch t {
  | Singleton(x) => x
  | Cons(x, _) => x
  }

let rec map = (t, f) =>
  switch t {
  | Singleton(x) => Singleton(f(x))
  | Cons(x, xs) => Cons(f(x), xs->map(f))
  }

let rec reduce' = (t, f) =>
  switch t {
  | Singleton(x) => x
  | Cons(x, xs) => f(xs->reduce'(f), x)
  }

let rec reduce = (t, z, f) =>
  switch t {
  | Singleton(x) => z(x)
  | Cons(x, xs) => f(xs->reduce(z, f), x)
  }

let rec foldr = (t, b, f) =>
  switch t {
  | Singleton(x) => f(x, b)
  | Cons(x, xs) => f(x, xs->foldr(b, f))
  }

let every = (t, p) => t->foldr(true, (x, b) => p(x) && b)

let allSome = t =>
  t->reduce(
    (a: option<'a>): option<t<'a>> =>
      switch a {
      | None => None
      | Some(a) => Some(Singleton(a))
      },
    (xs: option<t<'a>>, x: option<'a>): option<t<'a>> =>
      xs->Option.flatMap(xs => x->Option.map(x => xs->cons(x))),
  )

let rec or_error_all = ts =>
  switch ts {
  | Singleton(a) => a->Or_error.map(singleton)
  | Cons(a, rest) =>
    Or_error.both((a, or_error_all(rest)))->Or_error.map(((a, rest)) => Cons(a, rest))
  }

let toJson = (t, jsonify) => t->toList->List.toJson(jsonify)

let fromJson = (json, decode) =>
  json
  ->List.fromJson(decode)
  ->Or_error.flatMap(l =>
    l->fromList->Or_error.fromOption_s("Non-empty list is empty when reading JSON")
  )
