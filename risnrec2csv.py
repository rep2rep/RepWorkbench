#!/usr/bin/env python3

import json
import sys
import csv
import zipfile
import pprint

def make_data(key, args, data):
    """ For each event, we tidy up the args into proper data to be handled later. """
    if key == "File.NewModel":
        return {**data, "model_id": args[0], "folder_path": args[1]}
    elif key == "File.NewFolder":
        return {**data, "folder_id": args[0], "folder_path": args[1]}
    elif key in {"File.DeleteModel", "File.Undo", "File.Redo", "File.FocusModel"}:
        return {**data, "model_id": args[0]}
    elif key == "File.DeleteFolder":
        return {**data, "folder_id": args[0]}
    elif key == "File.DuplicateModel":
        return {**data, "model_id": args[0], "new_model_id": args[1]}
    elif key == "File.ImportModel":
        return {**data, "model_id": args[0], "model": args[1], "folder_path": args[2]}
    elif key == "File.ReorderModels":
        return {**data, "ordering": args[0]}
    elif key == "File.RenameFolder":
        return {**data, "folder_id": args[0], "name": args[1]}
    elif key == "File.ViewTransform":
        data = {**data, "model_id": args[0], **args[1]}
        del data["version"]
        return data
    elif key == "File.Intelligence.Response":
        data = {**data, **args[0]}
        return data
    elif key == "File.Intelligence.Focus":
        return {**data, "model_id": args[0], "intel_id": args[1]}
    elif key == "Model.Rename":
        return {**data, "name": args[0]}
    elif key == "Model.SetNotes":
        return {**data, "notes": args[0]}
    elif key == "Model.CreateNode":
        return {**data, "node_id": args[0], "x": args[1], "y": args[2], "kind": args[3]}
    elif key == "Model.DeleteNode" or key == "Model.Graph.DeleteNode":
        return {**data, "node_id": args[0]}
    elif key == "Model.Duplicate" or key == "Model.Graph.Duplicate":
        return {**data, "id_map": args[0]}
    elif key == "Model.LinkNodes" or key == "Model.Graph.LinkNodes":
        return {**data,
                "link_id": args[0],
                "source_node_id": args[1],
                "target_node_id": args[2],
                "kind": args[3],
                "order": args[4]}
    elif key == "Model.DeleteLink" or key == "Model.Graph.DeleteLink":
        return {**data, "link_id": args[0]}
    elif key == "Model.Graph.AddNode":
        return {**data, **args[0]}
    elif key == "Model.Graph.MoveNode":
        return {**data, "node_id": args[0], "x": args[1], "y": args[2]}
    elif key == "Model.Graph.SetSelection":
        return {**data, **args[0]}
    elif key.startswith("Model.Graph.UpdateNode.") \
         or key.startswith("Model.Graph.UpdateLink.") \
         or key.startswith("Model.Slots."):
        return {**data, "value": args[0]}
    return {**data, "args": args}


def normalise_events(time, prefix, event_json, data):
    """ Nested events are a bit icky. This turns them into a nice sequence! """
    [key, args] = event_json
    if prefix == "" and key == "File":
        return normalise_events(time, key, args[0], data)
    elif prefix == "" and key == "Model":
        return normalise_events(time, key, args[1], {**data, "model_id": args[0]})
    elif prefix == "File" and key == "Intelligence":
        return normalise_events(time, prefix + "." + key, args[0], data)
    elif prefix == "File.Intelligence" and key == "Response":
        # We need to handle "Intelligence Responses" carefully to 'smear' them for easy searching.
        response = args[0]
        events = []
        base = {
            "id": response["id"],
            "model_id": response["model"],
            "killed": response["killed"],
        }
        for e in response["errors"]:
            events.append({**base, "kind": "error", "intel": e})
        for w in response["warnings"]:
            events.append({**base, "kind": "warning", "intel": w})
        for i in response["insights"]:
            events.append({**base, "kind": "insight", "intel": i})
        return [(time, prefix + "." + key, make_data(prefix + "." + key, [ev], data)) for ev in events]
    elif prefix == "Model" and key == "Seq":
        return [e2 for e in args for e2 in normalise_events(time, prefix, e, data)]
    elif prefix == "Model" and key == "Graph":
        return normalise_events(time, prefix + "." + key, args[0], data)
    elif prefix == "Model" and key == "Slots":
        return normalise_events(time, prefix + "." + key, args[1], {**data, "inspected_id": args[0]})
    elif prefix == "Model.Graph" and key == "Seq":
        return [e2 for e in args for e2 in normalise_events(time, prefix, e, data)]
    elif prefix == "Model.Graph" and key == "UpdateNode":
        return normalise_events(time, prefix + "." + key, args[1], {**data, "node_id": args[0]})
    elif prefix == "Model.Graph" and key == "UpdateLink":
        return normalise_events(time, prefix + "." + key, args[1], {**data, "link_id": args[0]})
    elif prefix == "Model.Slots":
        return normalise_events(time, prefix + "." + key, args[0], data)
    else:
        key = prefix + "." + key
        return [(time, key, make_data(key, args, data))]


def dedup_intel(events):
    """ Intel events are spread across time for easier search, but this tends
        to result in A LOT of duplication. We find 'runs' of intel events with
        the same ID, and remove all but the first occurrence in that run.
    """
    # Runs is a nested dict. For each "response" event, we store the event indices
    # of when each event is seen, recording the first time individually, then
    # all the duplicates.
    runs = {}
    for i, (_, key, data) in enumerate(events):
        if key == "File.Intelligence.Response":
            if data["id"] not in runs:
                runs[data["id"]] = {}
            if data["intel"]["id"] not in runs[data["id"]]:
                runs[data["id"]][data["intel"]["id"]] = (i, set())
            else:
                runs[data["id"]][data["intel"]["id"]][1].add(i)
    # We now go through and filter out every event that is a "duplicate"
    dups = set(i for res in runs.values() for ev in res.values() for i in ev[1])
    return [ev for i, ev in enumerate(events) if i not in dups]

def normalise_events_seq(events_json):
    """ Flatten the sequence into a nice list of events """
    events = []
    for [time, event] in events_json:
       events.extend(normalise_events(time, "", event, {}))
    return dedup_intel(events)


def find_known_models(state_json, events_json):
    """ Models are known because they either exist from the beginning,
        or we make, them, or we import them, or we duplicate them. """
    models = {}
    for (gid, data) in state_json["models"].items():
        models[gid] = [(0, state_json["models"][gid]["info"]["name"])]
    for [time, key, args] in events_json:
        if key == "File.NewModel":
            models[args["model_id"]] = [(time, "Model")]
        elif key == "Model.Rename":
            models[args["model_id"]].append((time, args["args"][0]))
        elif key == "File.DuplicateModel":
            models[args["new_model_id"]] = [(time, models[args["model_id"]][-1][1])]
        elif key == "File.ImportModel":
            models[args["model_id"]] = [(time, args["model"]["info"]["name"])]
    return models


def make_models_csv(models, filename):
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f, quoting=csv.QUOTE_ALL)
        writer.writerow(["model_id", "created_at", "name"])
        for (gid, states) in models.items():
            for (time, name) in states:
                writer.writerow([gid, time, name])


def make_event_csv(events, filename):
    maxlen = 0
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f, quoting=csv.QUOTE_ALL)
        writer.writerow(["timestamp", "event", "model_id", "data"])
        for (timestamp, event, data) in events:
            if "model_id" in data:
                model_id = data["model_id"]
                data = {**data}
                del data["model_id"]
            else:
                model_id = None
            d = json.dumps(data)
            maxlen = max(maxlen, len(d))
            writer.writerow([timestamp, event, model_id, d])
    if maxlen > 32767:
        print("CSV HAS CELLS TOO LARGE FOR EXCEL.")
        print("EXCEL VERSION CREATED WITH MISSING DATA.")
    with open(filename.replace(".csv", "_EXCEL.csv"), 'w', newline='') as f:
        writer = csv.writer(f, quoting=csv.QUOTE_ALL)
        writer.writerow(["timestamp", "event", "model_id", "data"])
        for (timestamp, event, data) in events:
            if "model_id" in data:
                model_id = data["model_id"]
                data = {**data}
                del data["model_id"]
            else:
                model_id = None
            d = json.dumps(data)
            if len(d) > 25000:
                writer.writerow([timestamp, event, model_id, "DATA OMITTED FOR EXCEL"])
            else:
                writer.writerow([timestamp, event, model_id, d])


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please specify the .risnrec file to convert to CSVs")
        sys.exit(1)
    with zipfile.ZipFile(sys.argv[1]) as z:
        with z.open("0") as f:
            raw = json.load(f)
    print("RISN Recording from Editor Version ", raw[0])
    events = normalise_events_seq(raw[2])
    models = find_known_models(raw[1], events)
    fname = sys.argv[1].rsplit(".", maxsplit=1)[0]
    make_models_csv(models, fname + "_models.csv")
    make_event_csv(events, fname + "_events.csv")
