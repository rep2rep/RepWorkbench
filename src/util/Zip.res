type t

module File = {
  type t

  @get external name: t => string = "name"
  @get external isFolder: t => bool = "dir"
  @get external date: t => Js.Date.t = "date"
  @get external _comment: t => Js.Nullable.t<string> = "comment"
  let comment = t => t->_comment->Js.Nullable.toOption

  @send external _async: (t, string) => Promise.t<'a> = "async"
  let text: t => Promise.t<string> = t => t->_async("text")
}

module Folder = {
  type t

  @send external createFile: (t, string, string) => unit = "file"
  @send external createFolder: (t, string) => t = "folder"

  @send external _get: (t, string) => Js.Nullable.t<File.t> = "file"
  let get: (t, string) => option<File.t> = (t, name) => t->_get(name)->Js.Nullable.toOption
  @get external _files: t => Js.Dict.t<File.t> = "files"
  let files: t => array<File.t> = t => t->_files->Js.Dict.values
  @send external forEach: (t, (string, File.t) => unit) => unit = "forEach"

  @send external remove: (t, string) => t = "remove"
}

@module("./jszip.min") @val external _jszip: bool = "_jszip"
let () = (_jszip === true)->ignore

@new external create: unit => t = "JSZip"

let root: t => Folder.t = Obj.magic

@send external _genAsync: (t, {..}) => Promise.t<'a> = "generateAsync"
let generateAsync: t => Promise.t<string> = t => t->_genAsync({"type": "base64"})

@send external _loadAsync: (t, Js.TypedArray2.ArrayBuffer.t, {..}) => Promise.t<t> = "loadAsync"
let loadAsync: Js.TypedArray2.ArrayBuffer.t => Promise.t<t> = data =>
  create()->_loadAsync(data, {"createFolders": true})
