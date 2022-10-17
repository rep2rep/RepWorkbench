type t

module File: {
  type t

  let name: t => string
  let isFolder: t => bool
  let date: t => Js.Date.t
  let comment: t => option<string>
  let text: t => Promise.t<string>
}

module Folder: {
  type t

  let createFile: (t, string, string) => unit
  let createFolder: (t, string) => t

  let get: (t, string) => option<File.t>
  let files: t => array<File.t>
  let forEach: (t, (string, File.t) => unit) => unit

  let remove: (t, string) => t
}

let create: unit => t

let root: t => Folder.t

let generateAsync: t => Promise.t<string>
let loadAsync: Js.TypedArray2.ArrayBuffer.t => Promise.t<t>
