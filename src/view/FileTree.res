type rec inner<'a> =
  | File('a)
  | Folder(string, Gid.t, array<inner<'a>>)
type t<'a> = array<inner<'a>>

module Stable = {
  module V1 = {
    type t<'a> = array<'a>

    let toJson = Array.toJson
    let fromJson = Array.fromJson
  }

  module V2 = {
    type t<'a> = t<'a>

    let v1_to_v2 = t => {
      Js.Console.log(t)
      t->Array.map(v => File(v))
    }

    let rec toJson_helper = (t, jsonify) =>
      switch t {
      | File(f) =>
        Js.Dict.fromArray([
          ("kind", String.toJson("file")),
          ("payload", jsonify(f)),
        ])->Js.Json.object_
      | Folder(name, id, files) =>
        Js.Dict.fromArray([
          ("kind", String.toJson("folder")),
          ("name", String.toJson(name)),
          ("id", Gid.toJson(id)),
          ("files", files->Array.toJson(v => toJson_helper(v, jsonify))),
        ])->Js.Json.object_
      }

    let toJson = (t, jsonify) =>
      Js.Dict.fromArray([
        ("version", Int.toJson(2)),
        ("files", t->Array.toJson(v => toJson_helper(v, jsonify))),
      ])->Js.Json.object_

    let rec fromJson_helper = (json, parse) =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode File Tree object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        getValue("kind", String.fromJson)->Or_error.flatMap(kind => {
          switch kind {
          | "file" => getValue("payload", parse)->Or_error.map(v => File(v))
          | "folder" => {
              let name = getValue("name", String.fromJson)
              let id = getValue("id", Gid.fromJson)
              let files = getValue("files", v =>
                v->Array.fromJson(v' => fromJson_helper(v', parse))
              )
              (name, id, files)
              ->Or_error.both3
              ->Or_error.map(((name, id, files)) => Folder(name, id, files))
            }
          | k => Or_error.error_ss(["Unknown File Tree kind ", k])
          }
        })
      })

    let fromJson = (json, parse) =>
      switch json->Js.Json.decodeObject {
      | None => V1.fromJson(json, parse)->Or_error.map(v1_to_v2)
      | Some(dict) => {
          let getValue = (key, reader) =>
            dict
            ->Js.Dict.get(key)
            ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
            ->Or_error.flatMap(reader)
          let version = getValue("version", Int.fromJson)
          switch version->Or_error.match {
          | Or_error.Err(_) => V1.fromJson(json, parse)->Or_error.map(v1_to_v2)
          | Or_error.Ok(2) =>
            getValue("files", v => v->Array.fromJson(v' => fromJson_helper(v', parse)))
          | Or_error.Ok(b) => Or_error.error_ss(["Unknown File Tree version ", Int.toString(b)])
          }
        }
      }
  }
}

module FileOrFolder = {
  type t<'a> =
    | File('a)
    | Folder(string, Gid.t)
    | EndFolder(string, Gid.t)
}

let asPaths = t => {
  let rec flatten' = t' =>
    switch t' {
    | File(t'') => [FileOrFolder.File(t'')]
    | Folder(name, id, ts) =>
      Array.concatMany([
        [FileOrFolder.Folder(name, id)],
        ts->Array.flatMap(v => flatten'(v)),
        [FileOrFolder.EndFolder(name, id)],
      ])
    }
  t->Array.flatMap(v => flatten'(v))
}

let flatten = t =>
  t
  ->asPaths
  ->Array.keepMap(f =>
    switch f {
    | FileOrFolder.File(v) => Some(v)
    | _ => None
    }
  )

let rec fromPaths_helper = (nodes, idx) => {
  let folder = []
  let stop = ref(false)
  let i = ref(idx)
  while !stop.contents && i.contents < Array.length(nodes) {
    let f = nodes[i.contents]->Option.getExn
    switch f {
    | FileOrFolder.File(v) => {
        folder->Js.Array2.push(File(v))->ignore
        i := i.contents + 1
      }
    | FileOrFolder.Folder(name, id) => {
        let (contents, i') = fromPaths_helper(nodes, i.contents + 1)
        folder->Js.Array2.push(Folder(name, id, contents))->ignore
        i := i'
      }
    | FileOrFolder.EndFolder(_, _) => {
        stop := true
        i := i.contents + 1
      }
    }
  }
  (folder, i.contents)
}

let fromPaths = paths => fromPaths_helper(paths, 0)->fst

let empty = () => []

let isEmpty = t => Array.length(t) === 0

let map = (t, f) => {
  let rec map' = t' =>
    switch t' {
    | File(t'') => t''->f->File
    | Folder(name, id, ts) => Folder(name, id, ts->Array.map(map'))
    }
  t->Array.map(map')
}

let renameFolder = (t, id, name) => {
  let rec map' = t' =>
    switch t' {
    | File(_) => t'
    | Folder(name', id', ts) =>
      if id === id' {
        Folder(name, id, ts)
      } else {
        Folder(name', id', ts->Array.map(map'))
      }
    }
  t->Array.map(map')
}

let insert_helper = (t, path, position, value) => {
  let rec insert' = (ts, path') =>
    switch (ts, path') {
    | ([], []) => Some([value])
    | ([], _) => None
    | (_, []) => {
        let position = if position < 0 {
          Array.length(ts) + position + 1
        } else {
          position
        }
        let before = ts->Array.slice(~offset=0, ~len=position)
        let after = ts->Array.sliceToEnd(position)
        Array.concatMany([before, [value], after])->Some
      }
    | (_, _) => {
        let (find, path'') = {
          let p = Array.copy(path')
          let f = Js.Array2.shift(p)->Option.getExn
          (f, p)
        }
        let succeeded = ref(false)
        let ts' = ts->Array.map(t' =>
          switch t' {
          | File(_) => t'
          | Folder(name, id, files) =>
            if id === find {
              insert'(files, path'')
              ->Option.map(files' => {
                succeeded := true
                Folder(name, id, files')
              })
              ->Option.getWithDefault(t')
            } else {
              t'
            }
          }
        )
        if succeeded.contents {
          Some(ts')
        } else {
          None
        }
      }
    }
  if path->Array.length === 0 {
    let position = if position < 0 {
      Array.length(t) + position + 1
    } else {
      position
    }
    let before = t->Array.slice(~offset=0, ~len=position)
    let after = t->Array.sliceToEnd(position)
    Array.concatMany([before, [value], after])->Some
  } else {
    insert'(t, path)
  }
}

let insertFile = (t, ~path, ~position, v) => insert_helper(t, path, position, File(v))
let newFolder = (t, ~path, ~position, ~name, ~id) => {
  Js.Console.log((t, path))
  insert_helper(t, path, position, Folder(name, id, []))
}

let remove_helper = (t, p, ~removeFolderContents) => {
  let rec remove' = t' =>
    switch t' {
    | File(v) =>
      if p(t') {
        ([], [v])
      } else {
        ([t'], [])
      }
    | Folder(name, id, files) =>
      if p(t') {
        if removeFolderContents {
          ([], flatten(files))
        } else {
          (files, [])
        }
      } else {
        let (files', removed') = files->Array.map(remove')->Array.unzip
        ([Folder(name, id, files'->Array.concatMany)], removed'->Array.concatMany)
      }
    }
  let (x, y) = t->Array.map(remove')->Array.unzip
  (Array.concatMany(x), Array.concatMany(y))
}

let removeFile = (t, p) =>
  remove_helper(
    t,
    v =>
      switch v {
      | File(v) => p(v)
      | _ => false
      },
    ~removeFolderContents=false,
  )->fst

let removeFolder = (t, id) =>
  remove_helper(
    t,
    v =>
      switch v {
      | Folder(_, id', _) => id === id'
      | _ => false
      },
    ~removeFolderContents=false,
  )->fst

let removeFolderAndContents = (t, id) =>
  remove_helper(
    t,
    v =>
      switch v {
      | Folder(_, id', _) => id === id'
      | _ => false
      },
    ~removeFolderContents=true,
  )

let getPathAndPosition_helper = (t, p) => {
  let rec path' = (ts, path) =>
    switch ts {
    | [] => None
    | _ =>
      ts->Array.reduceWithIndex(None, (found, t, position) => {
        switch (found, t) {
        | (Some(_), _) => found
        | (None, File(_) as v) =>
          if p(v) {
            Some((path, position))
          } else {
            None
          }
        | (None, Folder(_, id, files) as v) =>
          if p(v) {
            Some((path, position))
          } else {
            path'(files, Array.concat(path, [id]))
          }
        }
      })
    }
  path'(t, [])
}

let getFilePathAndPosition = (t, p) =>
  getPathAndPosition_helper(t, v =>
    switch v {
    | File(v) => p(v)
    | _ => false
    }
  )

let getFolderPathAndPosition = (t, id) =>
  getPathAndPosition_helper(t, v =>
    switch v {
    | Folder(_, id', _) => id === id'
    | _ => false
    }
  )

let getFolder = (t, id) => {
  let rec folder' = ts =>
    switch ts {
    | [] => None
    | _ =>
      ts->Array.reduce(None, (found, t) => {
        switch (found, t) {
        | (Some(_), _) => found
        | (None, File(_)) => None
        | (None, Folder(_, id', files) as v) =>
          if id === id' {
            Some(v)
          } else {
            folder'(files)
          }
        }
      })
    }
  folder'(t)
}

let folderIsEmpty = (t, id) =>
  t
  ->getFolder(id)
  ->Option.flatMap(f =>
    switch f {
    | Folder(_, _, files) => Some(Array.length(files) === 0)
    | _ => None
    }
  )

let folderContents = (t, id) =>
  t
  ->getFolder(id)
  ->Option.flatMap(f =>
    switch f {
    | Folder(_, _, files) => Some(files)
    | _ => None
    }
  )
