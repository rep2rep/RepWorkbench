type document

@val external document: document = "document"
@send external createElement: (document, string) => Dom.element = "createElement"
@send external setAttribute: (Dom.element, string, string) => unit = "setAttribute"
@send external click: (Dom.element, unit) => unit = "click"

let download = (name, content) => {
  let a = document->createElement("a")
  a->setAttribute("href", content)
  a->setAttribute("download", name)
  a->click()
}
