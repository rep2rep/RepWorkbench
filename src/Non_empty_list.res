type rec t<'a> =
  | Singleton('a)
  | Cons('a, t<'a>)

let rec of_list = xs => {
  switch xs {
  | list{} => None
  | list{x} => Some(Singleton(x))
  | list{x, ...rest} => Some(Cons(x, Option.getExn(of_list(rest))))
  }
}

let rec to_list = t => {
  switch t {
  | Singleton(x) => list{x}
  | Cons(x, xs) => list{x, ...to_list(xs)}
  }
}
