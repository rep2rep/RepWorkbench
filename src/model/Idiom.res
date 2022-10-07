module Node = {
  type t =
    | Any
    | Representation
    | Scheme
    | Dimension
    | Token({is_class: bool})
    | Placeholder
}

module Link = {
  type t =
    | Any
    | Hierarchy
    | Anchor
    | Generic
}

type graph = (Gid.Map.t<InspectorState.Schema.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

type base = (Gid.Map.t<Node.t>, array<(Gid.t, Gid.t, Link.t)>)
type t<'kind> = {
  base: base,
  expand: (graph, Gid.Map.t<Gid.t>) => graph,
  reject: (graph, Gid.Map.t<Gid.t>) => bool,
}

let nodes = t => {
  let (nodes, _) = t.base
  nodes
}
let links = t => {
  let (_, links) = t.base
  links
}
let matchMaximal = (t, graph, mapping) => t.expand(graph, mapping)
let reject = (t, graph, mapping) => t.reject(graph, mapping)

type sumDimension
type prodDimension
type pickCollection
type filterCollection
type forEachCollection
type reduceCollection
type explicitCoordinateSystem
type implicitCoordinateSystem

let sumDimension = {
  let nSubs = 2
  let parent = Gid.create()
  let children = Array.range(1, nSubs)->Array.map(_ => Gid.create())
  let links = children->Array.map(child => (parent, child, Link.Hierarchy))
  let base = (
    Array.concatMany([
      [(parent, Node.Dimension)],
      children->Array.map(id => (id, Node.Dimension)),
    ])->Gid.Map.fromArray,
    links,
  )
  let expand = ((gnodes, glinks), mapping) => {
    let parentIso = mapping->Gid.Map.get(parent)->Option.getExn
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, tgt, kind) = lnk
      if (
        kind === ModelLink.Kind.Hierarchy &&
        src === parentIso &&
        gnodes
        ->Gid.Map.get(tgt)
        ->Option.map(schema =>
          switch schema {
          | InspectorState.Schema.Dimension(_) => true
          | _ => false
          }
        )
        ->Option.getWithDefault(false)
      ) {
        Some(lnk)
      } else {
        None
      }
    })
    let nDimParents = id =>
      glinks
      ->Array.keep(((src, tgt, _)) =>
        tgt === id &&
          gnodes
          ->Gid.Map.get(src)
          ->Option.map(sch =>
            switch sch {
            | InspectorState.Schema.Dimension(_) => true
            | _ => false
            }
          )
          ->Option.getWithDefault(false)
      )
      ->Array.length
    let childrenIso =
      allLinks
      ->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
      ->Array.keep(((child, _)) => nDimParents(child) == 1)
    (
      Array.concat(
        [(parentIso, gnodes->Gid.Map.get(parentIso)->Option.getExn)],
        childrenIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  let reject = ((gnodes, glinks), mapping) => {
    let nDimParents = id =>
      glinks
      ->Array.keep(((src, tgt, _)) =>
        tgt === id &&
          gnodes
          ->Gid.Map.get(src)
          ->Option.map(sch =>
            switch sch {
            | InspectorState.Schema.Dimension(_) => true
            | _ => false
            }
          )
          ->Option.getWithDefault(false)
      )
      ->Array.length
    let childrenIso = children->Array.keepMap(child => mapping->Gid.Map.get(child))
    childrenIso->Array.some(child => nDimParents(child) != 1)
  }
  {base: base, expand: expand, reject: reject}
}

let prodDimension = {
  let nProds = 2
  let parents = Array.range(1, nProds)->Array.map(_ => Gid.create())
  let child = Gid.create()
  let links = parents->Array.map(p => (p, child, Link.Hierarchy))
  let base = (
    Array.concatMany([
      parents->Array.map(id => (id, Node.Dimension)),
      [(child, Node.Dimension)],
    ])->Gid.Map.fromArray,
    links,
  )
  let expand = ((gnodes, glinks), mapping) => {
    let childIso = mapping->Gid.Map.get(child)->Option.getExn
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, tgt, kind) = lnk
      if (
        kind === ModelLink.Kind.Hierarchy &&
        tgt === childIso &&
        gnodes
        ->Gid.Map.get(src)
        ->Option.map(schema =>
          switch schema {
          | InspectorState.Schema.Dimension(_) => true
          | _ => false
          }
        )
        ->Option.getWithDefault(false)
      ) {
        Some(lnk)
      } else {
        None
      }
    })
    let parentsIso =
      allLinks->Array.map(((parent, _, _)) => (parent, gnodes->Gid.Map.get(parent)->Option.getExn))
    (
      Array.concat(
        [(childIso, gnodes->Gid.Map.get(childIso)->Option.getExn)],
        parentsIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  {base: base, expand: expand, reject: (_, _) => false}
}

let pickCollection = {
  let nPicks = 1
  let parent = Gid.create()
  let classChild = Gid.create()
  let children = Array.range(1, nPicks)->Array.map(_ => Gid.create())
  let links = Array.concat(
    [(parent, classChild, Link.Hierarchy)],
    children->Array.map(child => (parent, child, Link.Hierarchy)),
  )
  let base = (
    Array.concat(
      [(parent, Node.Dimension), (classChild, Node.Token({is_class: true}))],
      children->Array.map(id => (id, Node.Token({is_class: false}))),
    )->Gid.Map.fromArray,
    links,
  )
  let expand = ((gnodes, glinks), mapping) => {
    let parentIso = mapping->Gid.Map.get(parent)->Option.getExn
    let classChildIso = mapping->Gid.Map.get(classChild)->Option.getExn
    let nParents = id => glinks->Array.keep(((_, tgt, _)) => tgt == id)->Array.length
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, tgt, kind) = lnk
      if (
        kind === ModelLink.Kind.Hierarchy &&
        src === parentIso &&
        (tgt === classChildIso ||
          gnodes
          ->Gid.Map.get(tgt)
          ->Option.map(schema =>
            switch schema {
            | InspectorState.Schema.Token(t) => !(t.is_class->Option.getWithDefault(false))
            | _ => false
            }
          )
          ->Option.getWithDefault(false))
      ) {
        Some(lnk)
      } else {
        None
      }
    })
    let childrenIso =
      allLinks
      ->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
      ->Array.keep(((id, _)) => nParents(id) == 1)
    (
      Array.concat(
        [(parentIso, gnodes->Gid.Map.get(parentIso)->Option.getExn)],
        childrenIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  let reject = ((_, glinks), mapping) => {
    let tgtIds = mapping->Gid.Map.values->Gid.Set.fromArray
    let children = glinks->Array.mapPartial(((_, tgt, _)) =>
      if tgtIds->Gid.Set.has(tgt) {
        Some(tgt)
      } else {
        None
      }
    )
    let nParents = id => glinks->Array.keep(((_, tgt, _)) => tgt == id)->Array.length
    children->Array.some(id => nParents(id) > 1)
  }
  {base: base, expand: expand, reject: reject}
}

let filterCollection = {
  let nFilters = 1
  let parent = Gid.create()
  let children = Array.range(1, nFilters)->Array.map(_ => (Gid.create(), Gid.create()))
  let links =
    children->Array.flatMap(((src, tgt)) => [
      (parent, src, Link.Hierarchy),
      (src, tgt, Link.Hierarchy),
    ])
  let base = (
    Array.concat(
      [(parent, Node.Dimension)],
      children->Array.flatMap(((src, tgt)) => [
        (src, Node.Dimension),
        (tgt, Node.Token({is_class: true})),
      ]),
    )->Gid.Map.fromArray,
    links,
  )
  let expand = ((gnodes, glinks), mapping) => {
    let parentIso = mapping->Gid.Map.get(parent)->Option.getExn
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, tgt, kind) = lnk
      if (
        kind === ModelLink.Kind.Hierarchy &&
        src === parentIso &&
        gnodes
        ->Gid.Map.get(tgt)
        ->Option.map(schema =>
          switch schema {
          | InspectorState.Schema.Dimension(_) =>
            glinks->Array.some(lnk' => {
              // Need to make sure that the subDimension is also a collection! So look for class token
              let (src', tgt', kind') = lnk'
              kind' === ModelLink.Kind.Hierarchy &&
              src' === tgt &&
              gnodes
              ->Gid.Map.get(tgt')
              ->Option.map(schema =>
                switch schema {
                | InspectorState.Schema.Token(t) => t.is_class->Option.getWithDefault(false)
                | _ => false
                }
              )
              ->Option.getWithDefault(false)
            })
          | _ => false
          }
        )
        ->Option.getWithDefault(false)
      ) {
        Some(lnk)
      } else {
        None
      }
    })
    let nDimParents = id =>
      glinks
      ->Array.keep(((src, tgt, _)) =>
        tgt === id &&
          gnodes
          ->Gid.Map.get(src)
          ->Option.map(sch =>
            switch sch {
            | InspectorState.Schema.Dimension(_) => true
            | _ => false
            }
          )
          ->Option.getWithDefault(false)
      )
      ->Array.length
    let children =
      allLinks
      ->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
      ->Array.keep(((child, _)) => nDimParents(child) === 1)
    (
      Array.concat(
        [(parentIso, gnodes->Gid.Map.get(parentIso)->Option.getExn)],
        children,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  let reject = ((gnodes, glinks), mapping) => {
    let nDimParents = id =>
      glinks
      ->Array.keep(((src, tgt, _)) =>
        tgt === id &&
          gnodes
          ->Gid.Map.get(src)
          ->Option.map(sch =>
            switch sch {
            | InspectorState.Schema.Dimension(_) => true
            | _ => false
            }
          )
          ->Option.getWithDefault(false)
      )
      ->Array.length
    let childrenIso = children->Array.keepMap(((child, _)) => mapping->Gid.Map.get(child))
    childrenIso->Array.some(child => nDimParents(child) != 1)
  }
  {base: base, expand: expand, reject: reject}
}

let forEachCollection = {
  let root = Gid.create()
  let intermediate = Gid.create()
  let child = Gid.create()
  let base = (
    [
      (root, Node.Dimension),
      (intermediate, Node.Token({is_class: true})),
      (child, Node.Any),
    ]->Gid.Map.fromArray,
    [(root, intermediate, Link.Hierarchy), (intermediate, child, Link.Anchor)],
  )
  let expand = ((gnodes, glinks), mapping) => {
    let rootIso = mapping->Gid.Map.get(root)->Option.getExn
    let intermediateIso = mapping->Gid.Map.get(intermediate)->Option.getExn
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, _, kind) = lnk
      if kind === ModelLink.Kind.Anchor && src === intermediateIso {
        Some(lnk)
      } else {
        None
      }
    })
    let childrenIso =
      allLinks->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
    (
      Array.concat(
        [
          (rootIso, gnodes->Gid.Map.get(rootIso)->Option.getExn),
          (intermediateIso, gnodes->Gid.Map.get(intermediateIso)->Option.getExn),
        ],
        childrenIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  {base: base, expand: expand, reject: (_, _) => false}
}

let reduceCollection = {
  let root = Gid.create()
  let intermediate = Gid.create()
  let child = Gid.create()
  let base = (
    [
      (root, Node.Token({is_class: false})),
      (intermediate, Node.Dimension),
      (child, Node.Token({is_class: true})),
    ]->Gid.Map.fromArray,
    [(root, intermediate, Link.Anchor), (intermediate, child, Link.Hierarchy)],
  )
  let expand = ((gnodes, glinks), mapping) => {
    let rootIso = mapping->Gid.Map.get(root)->Option.getExn
    let intermediateIso = mapping->Gid.Map.get(intermediate)->Option.getExn
    let allLinks =
      glinks->Array.keep(((src, tgt, kind)) =>
        kind === ModelLink.Kind.Anchor && src === rootIso && tgt === intermediateIso
      )
    (
      [
        (rootIso, gnodes->Gid.Map.get(rootIso)->Option.getExn),
        (intermediateIso, gnodes->Gid.Map.get(intermediateIso)->Option.getExn),
      ]->Gid.Map.fromArray,
      allLinks,
    )
  }
  let reject = ((_, glinks), mapping) => {
    let intermediateIso = mapping->Gid.Map.get(intermediate)->Option.getExn
    let intermediateParents = glinks->Array.keep(((_, tgt, _)) => tgt == intermediateIso)
    intermediateParents->Array.length != 1
  }
  {base: base, expand: expand, reject: reject}
}

let implicitCoordinateSystem = {
  let nDims = 2
  let parent = Gid.create()
  let children = Array.range(1, nDims)->Array.map(_ => Gid.create())
  let links = children->Array.map(child => (parent, child, Link.Hierarchy))
  let base = (
    Array.concatMany([
      [(parent, Node.Scheme)],
      children->Array.map(id => (id, Node.Dimension)),
    ])->Gid.Map.fromArray,
    links,
  )
  let expand = ((gnodes, glinks), mapping) => {
    let parentIso = mapping->Gid.Map.get(parent)->Option.getExn
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, tgt, kind) = lnk
      if (
        kind === ModelLink.Kind.Hierarchy &&
        src === parentIso &&
        gnodes
        ->Gid.Map.get(tgt)
        ->Option.map(schema =>
          switch schema {
          | InspectorState.Schema.Dimension(_) => true
          | _ => false
          }
        )
        ->Option.getWithDefault(false)
      ) {
        Some(lnk)
      } else {
        None
      }
    })
    let childrenIso =
      allLinks->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
    (
      Array.concat(
        [(parentIso, gnodes->Gid.Map.get(parentIso)->Option.getExn)],
        childrenIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  let reject = ((gnodes, glinks), mapping) => {
    let parentIso = mapping->Gid.Map.get(parent)
    let allChildren = glinks->Array.keepMap(((src, tgt, _)) =>
      parentIso->Option.flatMap(p =>
        if src === p {
          Some(tgt)
        } else {
          None
        }
      )
    )
    let isDim = id =>
      gnodes
      ->Gid.Map.get(id)
      ->Option.map(schema =>
        switch schema {
        | InspectorState.Schema.Dimension(_) => true
        | _ => false
        }
      )
      ->Option.getWithDefault(false)
    allChildren->Array.some(n => !isDim(n))
  }
  {base: base, expand: expand, reject: reject}
}

let explicitCoordinateSystem = {
  let coords = 1
  let data = 1
  let root = Gid.create()
  let coordRoot = Gid.create()
  let coordChildren = Array.range(1, coords)->Array.map(_ => Gid.create())
  let dataChildren = Array.range(1, data)->Array.map(_ => Gid.create())
  let coordLinks = coordChildren->Array.map(child => (coordRoot, child, Link.Hierarchy))
  let rootLinks = Array.concat(
    [(root, coordRoot, Link.Hierarchy)],
    dataChildren->Array.map(child => (root, child, Link.Hierarchy)),
  )
  let base = (
    Array.concatMany([
      [(root, Node.Scheme), (coordRoot, Node.Scheme)],
      coordChildren->Array.map(id => (id, Node.Dimension)),
      dataChildren->Array.map(id => (id, Node.Dimension)),
    ])->Gid.Map.fromArray,
    Array.concat(coordLinks, rootLinks),
  )
  let expand = ((gnodes, glinks), mapping) => {
    let rootIso = mapping->Gid.Map.get(root)->Option.getExn
    let coordRootIso = mapping->Gid.Map.get(coordRoot)->Option.getExn
    let allLinks = glinks->Array.mapPartial(lnk => {
      let (src, tgt, kind) = lnk
      if (
        kind == ModelLink.Kind.Hierarchy &&
        (src === coordRootIso || src === rootIso) &&
        gnodes
        ->Gid.Map.get(tgt)
        ->Option.map(schema =>
          switch schema {
          | InspectorState.Schema.Dimension(_) => true
          | _ => false
          }
        )
        ->Option.getWithDefault(false)
      ) {
        Some(lnk)
      } else if kind == ModelLink.Kind.Hierarchy && src === rootIso && tgt === coordRootIso {
        Some(lnk)
      } else {
        None
      }
    })
    let childrenIso =
      allLinks->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
    (
      Array.concat(
        [(rootIso, gnodes->Gid.Map.get(rootIso)->Option.getExn)],
        childrenIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  {base: base, expand: expand, reject: (_, _) => false}
}
