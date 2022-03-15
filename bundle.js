import * as fs from "fs";
import * as cp from "child_process";
import * as esbuild from "esbuild";

const { readdirSync } = fs;

import * as path from "path";

let src = path.join(path.resolve("."), "src");
let dist = path.join(path.resolve("."), "dist");

const mode = "debug";

const cfg = {
  bundle: true,
  minify: mode === "release",
  sourcemap: mode !== "release",
  target: 'es2020',
  format: 'iife',
  treeShaking: true,
};

esbuild.build({
  ...cfg,
  entryPoints: [path.join(src, "App.bs.js")],
  outfile: path.join(dist, "main.js"),
});

esbuild.build({
  ...cfg,
  entryPoints: [path.join(src, "Intelligence.bs.js")],
  outfile: path.join(dist, "worker.js"),
})
