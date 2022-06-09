module Node = {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token({is_class: bool})
    | Placeholder
}

module Link = {
  type t =
    | Hierarchy
    | Anchor
    | Relation
    | Overlap
    | Disjoint
    | Generic
}

type graph = (Gid.Map.t<InspectorState.Schema.t>, array<(Gid.t, Gid.t, ModelLink.Kind.t)>)

type base = (Gid.Map.t<Node.t>, array<(Gid.t, Gid.t, Link.t)>)
type t<'kind> = {
  base: base,
  expand: (graph, Gid.Map.t<Gid.t>) => graph,
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
  {base: base, expand: expand}
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
  {base: base, expand: expand}
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
      allLinks->Array.map(((_, child, _)) => (child, gnodes->Gid.Map.get(child)->Option.getExn))
    (
      Array.concat(
        [(parentIso, gnodes->Gid.Map.get(parentIso)->Option.getExn)],
        childrenIso,
      )->Gid.Map.fromArray,
      allLinks,
    )
  }
  {base: base, expand: expand}
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
  {base: base, expand: expand}
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
  {base: base, expand: expand}
}
