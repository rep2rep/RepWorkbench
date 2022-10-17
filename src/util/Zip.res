type t

module Folder = {
  type t

  @send external file: (t, string, string) => unit = "file"
  @send external folder: (t, string) => t = "folder"
}

@module("./jszip.min") @val external _jszip: 'a => unit = "_jszip"

@new external create: unit => t = "JSZip"

let root: t => Folder.t = Obj.magic

@send external _genAsync: (t, {..}) => Promise.t<'a> = "generateAsync"
let generateAsync: t => Promise.t<string> = t => t->_genAsync({"type": "base64"})
