type t

@new external create: unit => t = "XMLSerializer"

@send external serializeToString: (t, Dom.element) => string = "serializeToString"
