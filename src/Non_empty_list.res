type rec t<'a> =
  | Singleton('a)
  | Cons('a, t<'a>)

let cons = (t, a) => Cons(a, t)

let rec of_list = xs =>
  switch xs {
  | list{} => None
  | list{x} => Some(Singleton(x))
  | list{x, ...rest} => Some(Cons(x, Option.getExn(of_list(rest))))
  }

let rec to_list = t =>
  switch t {
  | Singleton(x) => list{x}
  | Cons(x, xs) => list{x, ...to_list(xs)}
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

let rec reduce = (t, f) =>
  switch t {
  | Singleton(x) => x
  | Cons(x, xs) => f(x, xs->reduce(f))
  }
