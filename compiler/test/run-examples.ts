import { buildAndRunProject, initProject } from "./runner.ts";

const content = Deno.readTextFileSync(`../playground/examples.out.json`);
const examples = JSON.parse(content);

for (const e of examples) {
  await initProject("/tmp/borgo-emit", e);
  const output = await buildAndRunProject();
  console.log(output);
}
