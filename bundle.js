import * as fs from "fs";
import * as path from "path";
import * as esbuild from "esbuild";

const { readdirSync } = fs;

let src = path.join(path.resolve("."), "src");
let dist = path.join(path.resolve("."), "dist");

const mode = process.env.NODE_ENV || "development";
console.log("Running as " + mode);

const cfg = {
  bundle: true,
  sourcemap: mode !== "production",
  target: 'es2020',
  treeShaking: true,
};

if (process.argv[2] === "bundle") {
  console.log("Bundling");
  esbuild.build({
    ...cfg,
    minify: false,
    format: 'cjs',
    external: ["react", "react-dom"],
    entryPoints: [path.join(src, "App.bs.js")],
    outfile: path.join(dist, "main.js"),
  });
  esbuild.build({
    ...cfg,
    minify: false,
    format: 'cjs',
    external: ["react", "react-dom"],
    entryPoints: [path.join(src, "Intelligence.bs.js")],
    outfile: path.join(dist, "worker.js"),
  });
} else if (process.argv[2] === "minify") {
  console.log("Minifying");
  esbuild.build({
    ...cfg,
    minify: mode === "production",
    allowOverwrite: true,
    format: 'iife',
    entryPoints: [path.join(dist, "main.js")],
    outfile: path.join(dist, "main.js"),
  });
  esbuild.build({
    ...cfg,
    minify: mode === "production",
    allowOverwrite: true,
    format: 'iife',
    entryPoints: [path.join(dist, "worker.js")],
    outfile: path.join(dist, "worker.js"),
  });
}
