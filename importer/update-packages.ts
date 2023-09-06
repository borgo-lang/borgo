import { runShell as runShellOriginal } from "../compiler/test/runner.ts";

Deno.chdir("./std");

const modules = JSON.parse(Deno.readTextFileSync("modules.json"));

async function runShell(cmd: any) {
  const { code, output, err } = await runShellOriginal(cmd);
  if (code > 0) {
    Deno.exit(err);
  }

  return output.trim();
}

async function runImporter(gopkg, dest) {
  // cmd: ,
}

///
/// -------------------------------
///

const GOROOT = await runShell(["go", "env", "GOROOT"]);

for (const m of modules) {
  if (m.name == "std") {
    continue;
  }

  const path = GOROOT + "/src/" + m.path;

  const file = m.path.split("/").pop();
  const dest = m.path + "/" + file + ".brg";

  const contents = await runShell(["just", "run-importer", "-folder", path]);

  Deno.mkdirSync(m.path, { recursive: true });
  Deno.writeTextFileSync(dest, contents);
  console.log(`Writing ${dest}`);
}
