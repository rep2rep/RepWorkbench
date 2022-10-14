let mkJson = (key, args) => [String.toJson(key), args->Array.toJson(j => j)]->Array.toJson(j => j)
let readJson = json =>
  json
  ->Array.fromJson(Or_error.create)
  ->Or_error.flatMap(arr =>
    switch arr {
    | [key, args] => (String.fromJson(key), Array.fromJson(args, Or_error.create))->Or_error.both
    | _ => Or_error.error_s("Malformed event JSON")
    }
  )

module Intelligence = {
  type t =
    | Init
    | Response(Intelligence_Intf.Response.t)
    | Focus(Gid.t, option<Gid.t>)

  let dispatch = (state, t) =>
    switch t {
    | Init => state
    | Response(response) =>
      state->State.updateModelBypassUndoRedo(response.model, m =>
        m->State.Model.setIntelligence(Some(response))
      )
    | Focus(model, id) =>
      state->State.updateModelBypassUndoRedo(model, m => m->State.Model.setFocusedIntelligence(id))
    }

  let toJson = t =>
    switch t {
    | Init => mkJson("Init", [])
    | Response(i) => mkJson("Response", [Intelligence_Intf.Response.toJson(i)])
    | Focus(model, id) => mkJson("Focus", [Gid.toJson(model), id->Option.toJson(Gid.toJson)])
    }

  let fromJson = json =>
    json
    ->readJson
    ->Or_error.flatMap(((key, args)) =>
      switch (key, args) {
      | ("Init", []) => Or_error.create(Init)
      | ("Response", [i]) => i->Intelligence_Intf.Response.fromJson->Or_error.map(i => Response(i))
      | ("Focus", [model, id]) =>
        (Gid.fromJson(model), id->Option.fromJson(Gid.fromJson))
        ->Or_error.both
        ->Or_error.map(((model, id)) => Focus(model, id))
      | _ => Or_error.error_s("Unrecognised Event.Intelligence.t JSON")
      }
    )
}

module File = {
  type t =
    | NewModel(Gid.t, FileTree.Path.t)
    | NewFolder(Gid.t, FileTree.Path.t)
    | DeleteModel(Gid.t)
    | DeleteFolder(Gid.t)
    | FocusModel(option<Gid.t>)
    | DuplicateModel(Gid.t, Gid.t)
    | ImportModel(Gid.t, State.Model.t, FileTree.Path.t)
    | ReorderModels(FileTree.t<Gid.t>)
    | RenameFolder(Gid.t, string)
    | Undo(Gid.t)
    | Redo(Gid.t)
    | ViewTransform(Gid.t, ViewTransform.t)
    | Intelligence(Intelligence.t)

  let dispatch = (state, t, ~atTime) =>
    switch t {
    | NewModel(id, path) => state->State.createModel(id, path, ~atTime)
    | NewFolder(id, path) => state->State.createFolder(id, path)
    | DeleteModel(id) => state->State.deleteModel(id)
    | DeleteFolder(id) => state->State.deleteFolder(id)
    | FocusModel(id) => state->State.focusModel(id)
    | DuplicateModel(existing, new_) => state->State.duplicateModel(~existing, ~new_, ~atTime)
    | ImportModel(id, model, path) => state->State.importModel(id, model, path, ~atTime)
    | ReorderModels(order) => state->State.reorderModels(order)
    | RenameFolder(id, name) => state->State.renameFolder(id, name)
    | Undo(id) => state->State.undo(id)
    | Redo(id) => state->State.redo(id)
    | ViewTransform(id, vt) => state->State.setViewTransform(id, vt)
    | Intelligence(i) => state->Intelligence.dispatch(i)
    }

  let toJson = t =>
    switch t {
    | NewModel(id, path) =>
      mkJson("NewModel", [Gid.toJson(id), FileTree.Path.Stable.V1.toJson(path)])
    | NewFolder(id, path) =>
      mkJson("NewFolder", [Gid.toJson(id), FileTree.Path.Stable.V1.toJson(path)])
    | DeleteModel(id) => mkJson("DeleteModel", [Gid.toJson(id)])
    | DeleteFolder(id) => mkJson("DeleteFolder", [Gid.toJson(id)])
    | FocusModel(id) => mkJson("FocusModel", [id->Option.toJson(Gid.toJson)])
    | DuplicateModel(existing, new_) =>
      mkJson("DuplicateModel", [Gid.toJson(existing), Gid.toJson(new_)])
    | ImportModel(id, model, path) =>
      mkJson(
        "ImportModel",
        [Gid.toJson(id), State.Model.Stable.V5.toJson(model), FileTree.Path.Stable.V1.toJson(path)],
      )
    | ReorderModels(tree) => mkJson("ReorderModels", [tree->FileTree.Stable.V2.toJson(Gid.toJson)])
    | RenameFolder(id, name) => mkJson("RenameFolder", [Gid.toJson(id), String.toJson(name)])
    | Undo(id) => mkJson("Undo", [Gid.toJson(id)])
    | Redo(id) => mkJson("Redo", [Gid.toJson(id)])
    | ViewTransform(id, vt) =>
      mkJson("ViewTransform", [Gid.toJson(id), ViewTransform.Stable.V1.toJson(vt)])
    | Intelligence(i) => mkJson("Intelligence", [Intelligence.toJson(i)])
    }

  let fromJson = json =>
    json
    ->readJson
    ->Or_error.flatMap(((key, args)) =>
      switch (key, args) {
      | ("NewModel", [id, path]) =>
        (Gid.fromJson(id), FileTree.Path.Stable.V1.fromJson(path))
        ->Or_error.both
        ->Or_error.map(((id, path)) => NewModel(id, path))
      | ("NewFolder", [id, path]) =>
        (Gid.fromJson(id), FileTree.Path.Stable.V1.fromJson(path))
        ->Or_error.both
        ->Or_error.map(((id, path)) => NewFolder(id, path))
      | ("DeleteModel", [id]) => id->Gid.fromJson->Or_error.map(id => DeleteModel(id))
      | ("DeleteFolder", [id]) => id->Gid.fromJson->Or_error.map(id => DeleteFolder(id))
      | ("FocusModel", [id]) =>
        id->Option.fromJson(Gid.fromJson)->Or_error.map(id => FocusModel(id))
      | ("DuplicateModel", [existing, new_]) =>
        (Gid.fromJson(existing), Gid.fromJson(new_))
        ->Or_error.both
        ->Or_error.map(((existing, new_)) => DuplicateModel(existing, new_))
      | ("ImportModel", [id, model, path]) =>
        (
          Gid.fromJson(id),
          State.Model.Stable.V5.fromJson(model),
          FileTree.Path.Stable.V1.fromJson(path),
        )
        ->Or_error.both3
        ->Or_error.map(((id, model, path)) => ImportModel(id, model, path))
      | ("ReorderModels", [tree]) =>
        tree->FileTree.Stable.V2.fromJson(Gid.fromJson)->Or_error.map(tree => ReorderModels(tree))
      | ("RenameFolder", [id, name]) =>
        (Gid.fromJson(id), String.fromJson(name))
        ->Or_error.both
        ->Or_error.map(((id, name)) => RenameFolder(id, name))
      | ("Undo", [id]) => id->Gid.fromJson->Or_error.map(id => Undo(id))
      | ("Redo", [id]) => id->Gid.fromJson->Or_error.map(id => Redo(id))
      | ("ViewTransform", [id, vt]) =>
        (Gid.fromJson(id), ViewTransform.Stable.V1.fromJson(vt))
        ->Or_error.both
        ->Or_error.map(((id, vt)) => ViewTransform(id, vt))
      | ("Intelligence", [i]) => i->Intelligence.fromJson->Or_error.map(i => Intelligence(i))
      | _ => Or_error.error_s("Unrecognised Event.File.t JSON")
      }
    )
}

module Slots = {
  module Representation = {
    type t =
      | Domain(string)
      | Display(string)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Domain(s) => {...state, InspectorState.Representation.domain: s}
      | Display(s) => {...state, InspectorState.Representation.display: s}
      | Notes(s) => {...state, InspectorState.Representation.notes: s}
      }

    let toJson = t =>
      switch t {
      | Domain(s) => mkJson("Domain", [String.toJson(s)])
      | Display(s) => mkJson("Display", [String.toJson(s)])
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Domain", [s]) => s->String.fromJson->Or_error.map(s => Domain(s))
        | ("Display", [s]) => s->String.fromJson->Or_error.map(s => Display(s))
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Representation.t JSON")
        }
      )
  }

  module Scheme = {
    type t =
      | Concept_structure(string)
      | Graphic_structure(string)
      | Function(option<Function.t>)
      | Explicit(option<bool>)
      | Scope(option<Scope.t>)
      | Organisation(string)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Concept_structure(s) => {...state, InspectorState.Scheme.concept_structure: s}
      | Graphic_structure(s) => {...state, InspectorState.Scheme.graphic_structure: s}
      | Function(s) => {...state, InspectorState.Scheme.function: s}
      | Explicit(s) => {...state, InspectorState.Scheme.explicit: s}
      | Scope(s) => {...state, InspectorState.Scheme.scope: s}
      | Organisation(s) => {...state, InspectorState.Scheme.organisation: s}
      | Notes(s) => {...state, InspectorState.Scheme.notes: s}
      }

    let toJson = t =>
      switch t {
      | Concept_structure(s) => mkJson("Concept_structure", [String.toJson(s)])
      | Graphic_structure(s) => mkJson("Graphic_structure", [String.toJson(s)])
      | Function(f) => mkJson("Function", [f->Option.toJson(Function.toJson)])
      | Explicit(e) => mkJson("Explicit", [e->Option.toJson(Bool.toJson)])
      | Scope(s) => mkJson("Scope", [s->Option.toJson(Scope.toJson)])
      | Organisation(s) => mkJson("Organisation", [String.toJson(s)])
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Concept_structure", [s]) => s->String.fromJson->Or_error.map(s => Concept_structure(s))
        | ("Graphic_structure", [s]) => s->String.fromJson->Or_error.map(s => Graphic_structure(s))
        | ("Function", [f]) => f->Option.fromJson(Function.fromJson)->Or_error.map(f => Function(f))
        | ("Explicit", [e]) => e->Option.fromJson(Bool.fromJson)->Or_error.map(e => Explicit(e))
        | ("Scope", [s]) => s->Option.fromJson(Scope.fromJson)->Or_error.map(s => Scope(s))
        | ("Organisation", [s]) => s->String.fromJson->Or_error.map(s => Organisation(s))
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Scheme.t JSON")
        }
      )
  }

  module Dimension = {
    type t =
      | Concept(string)
      | Concept_scale(option<Quantity_scale.t>)
      | Concept_attributes(list<string>)
      | Graphic(string)
      | Graphic_scale(option<Quantity_scale.t>)
      | Graphic_attributes(list<string>)
      | Function(option<Function.t>)
      | Scope(option<Scope.t>)
      | Explicit(option<bool>)
      | Organisation(string)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Concept(s) => {...state, InspectorState.Dimension.concept: s}
      | Concept_scale(s) => {...state, InspectorState.Dimension.concept_scale: s}
      | Concept_attributes(s) => {...state, InspectorState.Dimension.concept_attributes: s}
      | Graphic(s) => {...state, InspectorState.Dimension.graphic: s}
      | Graphic_scale(s) => {...state, InspectorState.Dimension.graphic_scale: s}
      | Graphic_attributes(s) => {...state, InspectorState.Dimension.graphic_attributes: s}
      | Function(s) => {...state, InspectorState.Dimension.function: s}
      | Scope(s) => {...state, InspectorState.Dimension.scope: s}
      | Explicit(s) => {...state, InspectorState.Dimension.explicit: s}
      | Organisation(s) => {...state, InspectorState.Dimension.organisation: s}
      | Notes(s) => {...state, InspectorState.Dimension.notes: s}
      }

    let toJson = t =>
      switch t {
      | Concept(s) => mkJson("Concept", [String.toJson(s)])
      | Concept_scale(s) => mkJson("Concept_scale", [s->Option.toJson(Quantity_scale.toJson)])
      | Concept_attributes(s) => mkJson("Concept_attributes", [s->List.toJson(String.toJson)])
      | Graphic(s) => mkJson("Graphic", [String.toJson(s)])
      | Graphic_scale(s) => mkJson("Graphic_scale", [s->Option.toJson(Quantity_scale.toJson)])
      | Graphic_attributes(s) => mkJson("Graphic_attributes", [s->List.toJson(String.toJson)])
      | Function(f) => mkJson("Function", [f->Option.toJson(Function.toJson)])
      | Explicit(e) => mkJson("Explicit", [e->Option.toJson(Bool.toJson)])
      | Scope(s) => mkJson("Scope", [s->Option.toJson(Scope.toJson)])
      | Organisation(s) => mkJson("Organisation", [String.toJson(s)])
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Concept", [s]) => s->String.fromJson->Or_error.map(s => Concept(s))
        | ("Concept_scale", [s]) =>
          s->Option.fromJson(Quantity_scale.fromJson)->Or_error.map(s => Concept_scale(s))
        | ("Concept_attributes", [s]) =>
          s->List.fromJson(String.fromJson)->Or_error.map(s => Concept_attributes(s))
        | ("Graphic", [s]) => s->String.fromJson->Or_error.map(s => Graphic(s))
        | ("Graphic_scale", [s]) =>
          s->Option.fromJson(Quantity_scale.fromJson)->Or_error.map(s => Graphic_scale(s))
        | ("Graphic_attributes", [s]) =>
          s->List.fromJson(String.fromJson)->Or_error.map(s => Graphic_attributes(s))
        | ("Function", [f]) => f->Option.fromJson(Function.fromJson)->Or_error.map(f => Function(f))
        | ("Explicit", [e]) => e->Option.fromJson(Bool.fromJson)->Or_error.map(e => Explicit(e))
        | ("Scope", [s]) => s->Option.fromJson(Scope.fromJson)->Or_error.map(s => Scope(s))
        | ("Organisation", [s]) => s->String.fromJson->Or_error.map(s => Organisation(s))
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Dimension.t JSON")
        }
      )
  }

  module Token = {
    type t =
      | Concept(string)
      | Graphic(string)
      | Is_class(option<bool>)
      | Function(option<Function.t>)
      | Explicit(option<bool>)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Concept(s) => {...state, InspectorState.Token.concept: s}
      | Graphic(s) => {...state, InspectorState.Token.graphic: s}
      | Is_class(s) => {...state, InspectorState.Token.is_class: s}
      | Function(s) => {...state, InspectorState.Token.function: s}
      | Explicit(s) => {...state, InspectorState.Token.explicit: s}
      | Notes(s) => {...state, InspectorState.Token.notes: s}
      }

    let toJson = t =>
      switch t {
      | Concept(s) => mkJson("Concept", [String.toJson(s)])
      | Graphic(s) => mkJson("Graphic", [String.toJson(s)])
      | Is_class(s) => mkJson("Is_class", [s->Option.toJson(Bool.toJson)])
      | Function(f) => mkJson("Function", [f->Option.toJson(Function.toJson)])
      | Explicit(e) => mkJson("Explicit", [e->Option.toJson(Bool.toJson)])
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Concept", [s]) => s->String.fromJson->Or_error.map(s => Concept(s))
        | ("Graphic", [s]) => s->String.fromJson->Or_error.map(s => Graphic(s))
        | ("Is_class", [s]) => s->Option.fromJson(Bool.fromJson)->Or_error.map(s => Is_class(s))
        | ("Function", [f]) => f->Option.fromJson(Function.fromJson)->Or_error.map(f => Function(f))
        | ("Explicit", [e]) => e->Option.fromJson(Bool.fromJson)->Or_error.map(e => Explicit(e))
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Token.t JSON")
        }
      )
  }

  module Placeholder = {
    type t =
      | Description(string)
      | IsIntensional(option<bool>)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Description(s) => {...state, InspectorState.Placeholder.description: s}
      | IsIntensional(s) => {...state, InspectorState.Placeholder.isIntensional: s}
      | Notes(s) => {...state, InspectorState.Placeholder.notes: s}
      }

    let toJson = t =>
      switch t {
      | Description(s) => mkJson("Description", [String.toJson(s)])
      | IsIntensional(s) => mkJson("IsIntensional", [s->Option.toJson(Bool.toJson)])
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Description", [s]) => s->String.fromJson->Or_error.map(s => Description(s))
        | ("IsIntensional", [s]) =>
          s->Option.fromJson(Bool.fromJson)->Or_error.map(s => IsIntensional(s))
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Placeholder.t JSON")
        }
      )
  }

  module Hierarchy = {
    type t =
      | Notes(string)
      | Order(option<int>)

    let dispatch = (state, t) =>
      switch t {
      | Notes(s) => {...state, InspectorState.Hierarchy.notes: s}
      | Order(o) => {...state, InspectorState.Hierarchy.order: o}
      }

    let toJson = t =>
      switch t {
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      | Order(s) => mkJson("Order", [s->Option.toJson(Int.toJson)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | ("Order", [s]) => s->Option.fromJson(Int.fromJson)->Or_error.map(s => Order(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Hierarchy.t JSON")
        }
      )
  }

  module Anchor = {
    type t =
      | Notes(string)
      | Order(option<int>)

    let dispatch = (state, t) =>
      switch t {
      | Notes(s) => {...state, InspectorState.Anchor.notes: s}
      | Order(o) => {...state, InspectorState.Anchor.order: o}
      }

    let toJson = t =>
      switch t {
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      | Order(s) => mkJson("Order", [s->Option.toJson(Int.toJson)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | ("Order", [s]) => s->Option.fromJson(Int.fromJson)->Or_error.map(s => Order(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Anchor.t JSON")
        }
      )
  }

  module Generic = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Generic.notes: s}
      }

    let toJson = t =>
      switch t {
      | Notes(s) => mkJson("Notes", [String.toJson(s)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("Notes", [s]) => s->String.fromJson->Or_error.map(s => Notes(s))
        | _ => Or_error.error_s("Unrecognised Event.Slots.Generic.t JSON")
        }
      )
  }

  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)
    | Placeholder(Placeholder.t)
    | Hierarchy(Hierarchy.t)
    | Anchor(Anchor.t)
    | Generic(Generic.t)

  let dispatch = (state, t) =>
    switch (state, t) {
    | (
        InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Representation(state)),
        Representation(e),
      ) =>
      Representation.dispatch(state, e)
      ->InspectorState.Schema.Representation
      ->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Scheme(state)), Scheme(e)) =>
      Scheme.dispatch(state, e)->InspectorState.Schema.Scheme->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Dimension(state)), Dimension(e)) =>
      Dimension.dispatch(state, e)
      ->InspectorState.Schema.Dimension
      ->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Token(state)), Token(e)) =>
      Token.dispatch(state, e)->InspectorState.Schema.Token->InspectorState.SchemaOrLink.Schema
    | (
        InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Placeholder(state)),
        Placeholder(e),
      ) =>
      Placeholder.dispatch(state, e)
      ->InspectorState.Schema.Placeholder
      ->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Hierarchy(state)), Hierarchy(e)) =>
      Hierarchy.dispatch(state, e)->InspectorState.Link.Hierarchy->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Anchor(state)), Anchor(e)) =>
      Anchor.dispatch(state, e)->InspectorState.Link.Anchor->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Generic(state)), Generic(e)) =>
      Generic.dispatch(state, e)->InspectorState.Link.Generic->InspectorState.SchemaOrLink.Link
    | _ => state
    }

  let toJson = t =>
    switch t {
    | Representation(r) => mkJson("Representation", [Representation.toJson(r)])
    | Scheme(s) => mkJson("Scheme", [Scheme.toJson(s)])
    | Dimension(d) => mkJson("Dimension", [Dimension.toJson(d)])
    | Token(t) => mkJson("Token", [Token.toJson(t)])
    | Placeholder(p) => mkJson("Placeholder", [Placeholder.toJson(p)])
    | Hierarchy(h) => mkJson("Hierarchy", [Hierarchy.toJson(h)])
    | Anchor(a) => mkJson("Anchor", [Anchor.toJson(a)])
    | Generic(g) => mkJson("Generic", [Generic.toJson(g)])
    }

  let fromJson = json =>
    json
    ->readJson
    ->Or_error.flatMap(((key, args)) =>
      switch (key, args) {
      | ("Representation", [r]) => r->Representation.fromJson->Or_error.map(r => Representation(r))
      | ("Scheme", [s]) => s->Scheme.fromJson->Or_error.map(s => Scheme(s))
      | ("Dimension", [d]) => d->Dimension.fromJson->Or_error.map(d => Dimension(d))
      | ("Token", [t]) => t->Token.fromJson->Or_error.map(t => Token(t))
      | ("Placeholder", [p]) => p->Placeholder.fromJson->Or_error.map(p => Placeholder(p))
      | ("Hierarchy", [h]) => h->Hierarchy.fromJson->Or_error.map(h => Hierarchy(h))
      | ("Anchor", [a]) => a->Anchor.fromJson->Or_error.map(a => Anchor(a))
      | ("Generic", [g]) => g->Generic.fromJson->Or_error.map(g => Generic(g))
      | _ => Or_error.error_s("Unrecognised Event.Slot.t JSON")
      }
    )
}

module Graph = {
  module Node = {
    type t =
      | UpdateName(string)
      | UpdateNameSuffix(option<string>)
      | UpdateReference(string)
      | UpdateReferenceSuffix(option<string>)
      | UpdateDashed(bool)

    let dispatch = (node, t) =>
      switch t {
      | UpdateName(name) => node->ModelNode.updatePayload(payload => {...payload, name: name})
      | UpdateNameSuffix(name_suffix) =>
        node->ModelNode.updatePayload(payload => {...payload, name_suffix: name_suffix})
      | UpdateReference(reference) =>
        node->ModelNode.updatePayload(payload => {...payload, reference: reference})
      | UpdateReferenceSuffix(reference_suffix) =>
        node->ModelNode.updatePayload(payload => {...payload, reference_suffix: reference_suffix})
      | UpdateDashed(dashed) =>
        node->ModelNode.updatePayload(payload => {...payload, dashed: dashed})
      }

    let toJson = t =>
      switch t {
      | UpdateName(s) => mkJson("UpdateName", [String.toJson(s)])
      | UpdateNameSuffix(s) => mkJson("UpdateNameSuffix", [s->Option.toJson(String.toJson)])
      | UpdateReference(s) => mkJson("UpdateReference", [String.toJson(s)])
      | UpdateReferenceSuffix(s) =>
        mkJson("UpdateReferenceSuffix", [s->Option.toJson(String.toJson)])
      | UpdateDashed(b) => mkJson("UpdateDashed", [Bool.toJson(b)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("UpdateName", [s]) => s->String.fromJson->Or_error.map(s => UpdateName(s))
        | ("UpdateNameSuffix", [s]) =>
          s->Option.fromJson(String.fromJson)->Or_error.map(s => UpdateNameSuffix(s))
        | ("UpdateReference", [s]) => s->String.fromJson->Or_error.map(s => UpdateReference(s))
        | ("UpdateReferenceSuffix", [s]) =>
          s->Option.fromJson(String.fromJson)->Or_error.map(s => UpdateReferenceSuffix(s))
        | ("UpdateDashed", [b]) => b->Bool.fromJson->Or_error.map(b => UpdateDashed(b))
        | _ => Or_error.error_s("Unrecognised Event.Graph.Node.t JSON")
        }
      )
  }

  module Link = {
    type t = UpdateOrder(option<int>)

    let dispatch = (link, t) =>
      switch t {
      | UpdateOrder(order) => link->ModelLink.updatePayload(ModelLink.Payload.setLabel(_, order))
      }

    let toJson = t =>
      switch t {
      | UpdateOrder(order) => mkJson("UpdateOrder", [order->Option.toJson(Int.toJson)])
      }

    let fromJson = json =>
      json
      ->readJson
      ->Or_error.flatMap(((key, args)) =>
        switch (key, args) {
        | ("UpdateOrder", [order]) =>
          order->Option.fromJson(Int.fromJson)->Or_error.map(order => UpdateOrder(order))
        | _ => Or_error.error_s("Unknown Event.Graph.Link.t JSON")
        }
      )
  }

  type rec t =
    | AddNode(ModelNode.t)
    | UpdateNode(Gid.t, Node.t)
    | UpdateLink(Gid.t, Link.t)
    | DeleteNode(Gid.t)
    | Duplicate(Gid.Map.t<Gid.t>)
    | MoveNode(Gid.t, float, float)
    | LinkNodes({
        linkId: Gid.t,
        source: Gid.t,
        target: Gid.t,
        kind: ModelLink.Kind.t,
        label: option<int>,
      })
    | DeleteLink(Gid.t)
    | SetSelection(ModelSelection.t)
    | Seq(array<t>)

  let rec dispatch = (state, t) =>
    switch t {
    | Seq(ts) => ts->Array.reduce(state, (state, t) => dispatch(state, t))
    | AddNode(node) => state->ModelState.addNode(node)
    | UpdateNode(id, e) =>
      state->ModelState.updateNodes(node =>
        if ModelNode.id(node) === id {
          Node.dispatch(node, e)
        } else {
          node
        }
      )
    | UpdateLink(id, e) =>
      state->ModelState.updateLinks(link =>
        if ModelLink.id(link) === id {
          Link.dispatch(link, e)
        } else {
          link
        }
      )
    | DeleteNode(id) => state->ModelState.removeNode(id)
    | Duplicate(idMap) => state->ModelState.duplicateNodes(idMap)
    | MoveNode(id, x, y) => state->ModelState.moveNode(id, ~x, ~y)
    | LinkNodes({linkId, source, target, kind, label}) => {
        let modelSource = state->ModelState.nodeWithId(source)
        let modelTarget = state->ModelState.nodeWithId(target)
        switch (modelSource, modelTarget) {
        | (Some(source), Some(target)) =>
          state->ModelState.addLink(ModelLink.create(~linkId, ~source, ~target, kind, ~label))
        | _ => state
        }
      }
    | DeleteLink(linkId) => state->ModelState.removeLink(linkId)
    | SetSelection(selection) => state->ModelState.setSelection(selection)
    }

  let rec toJson = t =>
    switch t {
    | AddNode(m) => mkJson("AddNode", [ModelNode.Stable.V2.toJson(m)])
    | UpdateNode(id, node) => mkJson("UpdateNode", [Gid.toJson(id), Node.toJson(node)])
    | UpdateLink(id, link) => mkJson("UpdateLink", [Gid.toJson(id), Link.toJson(link)])
    | DeleteNode(id) => mkJson("DeleteNode", [Gid.toJson(id)])
    | Duplicate(map) => mkJson("Duplicate", [map->Gid.Map.toJson(Gid.toJson)])
    | MoveNode(id, x, y) => mkJson("MoveNode", [Gid.toJson(id), Float.toJson(x), Float.toJson(y)])
    | LinkNodes({linkId, source, target, kind, label}) =>
      mkJson(
        "LinkNodes",
        [
          Gid.toJson(linkId),
          Gid.toJson(source),
          Gid.toJson(target),
          ModelLink.Kind.Stable.V3.toJson(kind),
          label->Option.toJson(Int.toJson),
        ],
      )
    | DeleteLink(id) => mkJson("DeleteLink", [Gid.toJson(id)])
    | SetSelection(s) => mkJson("SetSelection", [ModelSelection.Stable.V1.toJson(s)])
    | Seq(ts) => mkJson("Seq", ts->Array.map(toJson))
    }

  let rec fromJson = json =>
    json
    ->readJson
    ->Or_error.flatMap(((key, args)) =>
      switch (key, args) {
      | ("AddNode", [m]) => m->ModelNode.Stable.V2.fromJson->Or_error.map(m => AddNode(m))
      | ("UpdateNode", [id, node]) =>
        (Gid.fromJson(id), Node.fromJson(node))
        ->Or_error.both
        ->Or_error.map(((id, node)) => UpdateNode(id, node))
      | ("UpdateLink", [id, link]) =>
        (Gid.fromJson(id), Link.fromJson(link))
        ->Or_error.both
        ->Or_error.map(((id, link)) => UpdateLink(id, link))
      | ("DeleteNode", [id]) => id->Gid.fromJson->Or_error.map(id => DeleteNode(id))
      | ("Duplicate", [map]) =>
        map->Gid.Map.fromJson(Gid.fromJson)->Or_error.map(map => Duplicate(map))
      | ("MoveNode", [id, x, y]) =>
        (Gid.fromJson(id), Float.fromJson(x), Float.fromJson(y))
        ->Or_error.both3
        ->Or_error.map(((id, x, y)) => MoveNode(id, x, y))
      | ("LinkNodes", [linkId, source, target, kind, label]) =>
        (
          Gid.fromJson(linkId),
          Gid.fromJson(source),
          Gid.fromJson(target),
          ModelLink.Kind.Stable.V3.fromJson(kind),
          label->Option.fromJson(Int.fromJson),
        )
        ->Or_error.both5
        ->Or_error.map(((linkId, source, target, kind, label)) => LinkNodes({
          linkId: linkId,
          source: source,
          target: target,
          kind: kind,
          label: label,
        }))
      | ("DeleteLink", [id]) => id->Gid.fromJson->Or_error.map(id => DeleteLink(id))
      | ("SetSelection", [s]) =>
        s->ModelSelection.Stable.V1.fromJson->Or_error.map(s => SetSelection(s))
      | ("Seq", ts) => ts->Array.map(fromJson)->Or_error.allArray->Or_error.map(ts => Seq(ts))
      | _ => Or_error.error_s("Unrecognised Event.Graph.t JSON")
      }
    )
}

module Model = {
  type rec t =
    | Rename(string)
    | SetNotes(string)
    | CreateNode(Gid.t, float, float, ModelNode.Kind.t)
    | DeleteNode(Gid.t)
    | Duplicate(Gid.Map.t<Gid.t>)
    | LinkNodes({
        linkId: Gid.t,
        source: Gid.t,
        target: Gid.t,
        kind: ModelLink.Kind.t,
        label: option<int>,
      })
    | DeleteLink(Gid.t)
    | Graph(Graph.t)
    | Slots(Gid.t, Slots.t)
    | Seq(array<t>)

  let rec dispatch = (state, t) =>
    switch t {
    | Seq(ts) => ts->Array.reduce(state, (state, t) => dispatch(state, t))
    | Rename(name) =>
      state->State.Model.updateInfo(
        state->State.Model.info->(i => {...i, InspectorState.Model.name: name}),
      )
    | SetNotes(notes) =>
      state->State.Model.updateInfo(
        state->State.Model.info->(i => {...i, InspectorState.Model.notes: notes}),
      )
    | CreateNode(id, x, y, kind) => {
        let slots = InspectorState.Schema.empty(kind)
        let name = InspectorState.Schema.name(slots)
        let reference = InspectorState.Schema.reference(slots)
        let node = ModelNode.create(~name, ~reference, ~x, ~y, kind, id)
        let graph = state->State.Model.graph->Graph.dispatch(Graph.AddNode(node))
        let allSlots =
          state->State.Model.slots->Gid.Map.set(id, InspectorState.SchemaOrLink.Schema(slots))
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | DeleteNode(id) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.DeleteNode(id))
        let allSlots = state->State.Model.slots->Gid.Map.remove(id)
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | Duplicate(idMap) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.Duplicate(idMap))
        let allSlots =
          idMap
          ->Gid.Map.toArray
          ->Array.reduce(state->State.Model.slots, (slots, (oldId, newId)) => {
            slots
            ->Gid.Map.get(oldId)
            ->Option.map(s => slots->Gid.Map.set(newId, InspectorState.SchemaOrLink.duplicate(s)))
            ->Option.getWithDefault(slots)
          })
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | LinkNodes({linkId, source, target, kind, label}) => {
        let slots = InspectorState.Link.empty(kind)->InspectorState.SchemaOrLink.Link
        let allSlots = state->State.Model.slots->Gid.Map.set(linkId, slots)
        let state = state->State.Model.updateSlots(allSlots)
        let graph =
          state
          ->State.Model.graph
          ->Graph.dispatch(
            Graph.LinkNodes({
              linkId: linkId,
              source: source,
              target: target,
              kind: kind,
              label: label,
            }),
          )
        state->State.Model.updateGraph(graph)
      }
    | DeleteLink(linkId) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.DeleteLink(linkId))
        let allSlots = state->State.Model.slots->Gid.Map.remove(linkId)
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | Graph(ev) => {
        let graph = state->State.Model.graph->Graph.dispatch(ev)
        state->State.Model.updateGraph(graph)
      }
    | Slots(id, ev) => {
        let slots = state->State.Model.slots
        let s = slots->Gid.Map.get(id)
        let s' = s->Option.map(Slots.dispatch(_, ev))
        let slots' = s'->Option.map(s' => slots->Gid.Map.set(id, s'))->Option.getWithDefault(slots)
        state->State.Model.updateSlots(slots')
      }
    }

  let rec graphEvent = t =>
    switch t {
    | Rename(_)
    | SetNotes(_)
    | CreateNode(_, _, _, _)
    | DeleteNode(_)
    | Duplicate(_)
    | DeleteLink(_)
    | LinkNodes(_) =>
      None
    | Seq(ts) => ts->Array.mapPartial(graphEvent)->Graph.Seq->Some
    | Graph(e) => Some(e)
    | Slots(id, e) => {
        let e' = switch e {
        | Slots.Representation(Slots.Representation.Domain(s))
        | Slots.Scheme(Slots.Scheme.Concept_structure(s))
        | Slots.Dimension(Slots.Dimension.Concept(s))
        | Slots.Token(Slots.Token.Concept(s))
        | Slots.Placeholder(Slots.Placeholder.Description(s)) =>
          Some(id => Graph.UpdateNode(id, Graph.Node.UpdateName(s)))
        | Slots.Representation(Slots.Representation.Display(s))
        | Slots.Scheme(Slots.Scheme.Graphic_structure(s))
        | Slots.Dimension(Slots.Dimension.Graphic(s))
        | Slots.Token(Slots.Token.Graphic(s)) =>
          Some(id => Graph.UpdateNode(id, Graph.Node.UpdateReference(s)))
        | Slots.Dimension(Slots.Dimension.Concept_scale(s)) =>
          Some(
            id => Graph.UpdateNode(
              id,
              Graph.Node.UpdateNameSuffix(
                Some(
                  s
                  ->Option.map(Quantity_scale.toString)
                  ->Option.getWithDefault("-")
                  ->String.slice(~from=0, ~to_=1),
                ),
              ),
            ),
          )
        | Slots.Dimension(Slots.Dimension.Graphic_scale(s)) =>
          Some(
            id => Graph.UpdateNode(
              id,
              Graph.Node.UpdateReferenceSuffix(
                Some(
                  s
                  ->Option.map(Quantity_scale.toString)
                  ->Option.getWithDefault("-")
                  ->String.slice(~from=0, ~to_=1),
                ),
              ),
            ),
          )
        | Slots.Token(Slots.Token.Is_class(b))
        | Slots.Placeholder(Slots.Placeholder.IsIntensional(b)) =>
          Some(id => Graph.UpdateNode(id, Graph.Node.UpdateDashed(b->Option.getWithDefault(false))))
        | Slots.Hierarchy(Slots.Hierarchy.Order(o))
        | Slots.Anchor(Slots.Anchor.Order(o)) =>
          Some(id => Graph.UpdateLink(id, Graph.Link.UpdateOrder(o)))
        | _ => None
        }
        e'->Option.map(f => f(id))
      }
    }

  let rec toJson = t =>
    switch t {
    | Rename(s) => mkJson("Rename", [String.toJson(s)])
    | SetNotes(s) => mkJson("SetNotes", [String.toJson(s)])
    | CreateNode(id, x, y, kind) =>
      mkJson(
        "CreateNode",
        [Gid.toJson(id), Float.toJson(x), Float.toJson(y), ModelNode.Kind.Stable.V2.toJson(kind)],
      )
    | DeleteNode(id) => mkJson("DeleteNode", [Gid.toJson(id)])
    | Duplicate(nodes) => mkJson("Duplicate", [nodes->Gid.Map.toJson(Gid.toJson)])
    | LinkNodes({linkId, source, target, kind, label}) =>
      mkJson(
        "LinkNodes",
        [
          Gid.toJson(linkId),
          Gid.toJson(source),
          Gid.toJson(target),
          ModelLink.Kind.Stable.V3.toJson(kind),
          Option.toJson(label, Int.toJson),
        ],
      )
    | DeleteLink(id) => mkJson("DeleteLink", [Gid.toJson(id)])
    | Graph(g) => mkJson("Graph", [Graph.toJson(g)])
    | Slots(id, s) => mkJson("Slots", [Gid.toJson(id), Slots.toJson(s)])
    | Seq(ts) => mkJson("Seq", ts->Array.map(toJson))
    }

  let rec fromJson = json =>
    json
    ->readJson
    ->Or_error.flatMap(((key, args)) =>
      switch (key, args) {
      | ("Rename", [s]) => s->String.fromJson->Or_error.map(s => Rename(s))
      | ("SetNotes", [s]) => s->String.fromJson->Or_error.map(s => SetNotes(s))
      | ("CreateNode", [id, x, y, kind]) =>
        (
          Gid.fromJson(id),
          Float.fromJson(x),
          Float.fromJson(y),
          ModelNode.Kind.Stable.V2.fromJson(kind),
        )
        ->Or_error.both4
        ->Or_error.map(((id, x, y, kind)) => CreateNode(id, x, y, kind))
      | ("DeleteNode", [id]) => id->Gid.fromJson->Or_error.map(id => DeleteNode(id))
      | ("Duplicate", [nodes]) =>
        nodes->Gid.Map.fromJson(Gid.fromJson)->Or_error.map(nodes => Duplicate(nodes))
      | ("LinkNodes", [linkId, source, target, kind, label]) =>
        (
          Gid.fromJson(linkId),
          Gid.fromJson(source),
          Gid.fromJson(target),
          ModelLink.Kind.Stable.V3.fromJson(kind),
          Option.fromJson(label, Int.fromJson),
        )
        ->Or_error.both5
        ->Or_error.map(((linkId, source, target, kind, label)) => LinkNodes({
          linkId: linkId,
          source: source,
          target: target,
          kind: kind,
          label: label,
        }))
      | ("DeleteLink", [id]) => id->Gid.fromJson->Or_error.map(id => DeleteLink(id))
      | ("Graph", [g]) => g->Graph.fromJson->Or_error.map(g => Graph(g))
      | ("Slots", [id, s]) =>
        (Gid.fromJson(id), Slots.fromJson(s))
        ->Or_error.both
        ->Or_error.map(((id, s)) => Slots(id, s))
      | ("Seq", ts) => ts->Array.map(fromJson)->Or_error.allArray->Or_error.map(ts => Seq(ts))
      | _ => Or_error.error_s("Unrecognised Event.Model.t JSON")
      }
    )
}

type t =
  | Model(Gid.t, Model.t)
  | File(File.t)

let dispatch = (state, t, ~atTime) =>
  switch t {
  | Model(id, ev) => state->State.updateModel(id, Model.dispatch(_, ev), ~atTime)
  | File(ev) => File.dispatch(state, ev, ~atTime)
  }

let rec shouldTriggerIntelligence = e =>
  switch e {
  | File(File.Intelligence(Intelligence.Init)) => true // MUST be true
  | Model(_, Model.Graph(Graph.SetSelection(_)))
  | Model(_, Model.Graph(Graph.MoveNode(_, _, _)))
  | Model(_, Model.Rename(_))
  | Model(_, Model.SetNotes(_))
  | Model(_, Model.Slots(_, Slots.Representation(Slots.Representation.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Scheme(Slots.Scheme.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Dimension(Slots.Dimension.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Token(Slots.Token.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Placeholder(Slots.Placeholder.Notes(_))))
  | File(File.ReorderModels(_))
  | File(File.NewFolder(_))
  | File(File.DeleteFolder(_))
  | File(File.RenameFolder(_, _))
  | File(File.ViewTransform(_, _))
  | File(File.Intelligence(_)) => false
  | Model(id, Model.Seq(vs)) =>
    vs->Array.map(v => Model(id, v))->Array.some(shouldTriggerIntelligence)
  | _ => true
  }

let toJson = t =>
  switch t {
  | Model(id, m) => mkJson("Model", [Gid.toJson(id), Model.toJson(m)])
  | File(f) => mkJson("File", [File.toJson(f)])
  }

let fromJson = json =>
  json
  ->readJson
  ->Or_error.flatMap(((key, args)) =>
    switch (key, args) {
    | ("Model", [id_j, m_j]) =>
      (Gid.fromJson(id_j), Model.fromJson(m_j))
      ->Or_error.both
      ->Or_error.map(((id, m)) => Model(id, m))
    | ("File", [f]) => File.fromJson(f)->Or_error.map(f => File(f))
    | _ => Or_error.error_s("Unrecognised Event.t JSON")
    }
  )
