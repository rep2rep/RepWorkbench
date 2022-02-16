import * as fs from "fs";
import * as cp from "child_process";
import * as esbuild from "esbuild";

const { readdirSync } = fs;

import * as path from "path";

let src = path.join(path.resolve("."), "src");
let dist = path.join(path.resolve("."), "dist");

let targets = [path.join(src, "App.bs.js")];

esbuild.build({
  entryPoints: targets,
  bundle: true,
  minify: false,
  sourcemap: true,
  outfile: path.join(dist, "main.js"),
});
