module type S = {
  type t<'a>
  let both3: ((t<'a>, t<'b>, t<'c>)) => t<('a, 'b, 'c)>
  let both4: ((t<'a>, t<'b>, t<'c>, t<'d>)) => t<('a, 'b, 'c, 'd)>
  let both5: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>)) => t<('a, 'b, 'c, 'd, 'e)>
  let both6: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>)) => t<('a, 'b, 'c, 'd, 'e, 'f)>
  let both7: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>)) => t<('a, 'b, 'c, 'd, 'e, 'f, 'g)>
  let both8: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>, t<'h>)) => t<(
    'a,
    'b,
    'c,
    'd,
    'e,
    'f,
    'g,
    'h,
  )>
  let both9: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>, t<'h>, t<'i>)) => t<(
    'a,
    'b,
    'c,
    'd,
    'e,
    'f,
    'g,
    'h,
    'i,
  )>
  let both10: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>, t<'h>, t<'i>, t<'j>)) => t<(
    'a,
    'b,
    'c,
    'd,
    'e,
    'f,
    'g,
    'h,
    'i,
    'j,
  )>
  let both11: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>, t<'h>, t<'i>, t<'j>, t<'k>)) => t<(
    'a,
    'b,
    'c,
    'd,
    'e,
    'f,
    'g,
    'h,
    'i,
    'j,
    'k,
  )>
  let both12: (
    (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>, t<'h>, t<'i>, t<'j>, t<'k>, t<'l>)
  ) => t<('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l)>
  let both13: (
    (t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>, t<'g>, t<'h>, t<'i>, t<'j>, t<'k>, t<'l>, t<'m>)
  ) => t<('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm)>
}
