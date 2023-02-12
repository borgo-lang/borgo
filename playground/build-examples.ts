import { decode, markdownToCodeblocks } from "../compiler/test/runner.ts";

const content = Deno.readTextFileSync("./examples.md");
const codeBlocks = markdownToCodeblocks(content);

const result = [];

for (const b of codeBlocks) {
  // Run rustfmt
  const cmd = Deno.run({ cmd: ["rustfmt"], stdout: "piped", stdin: "piped" });

  await cmd.stdin.write(new TextEncoder().encode(b.code));
  await cmd.stdin.close();

  const status = await cmd.status();
  const output = decode(await cmd.output());

  if (status.code > 0) {
    console.log(output);
    Deno.exit(1);
  }

  result.push({ title: b.description, code: output });
}

Deno.writeTextFileSync("./static/examples.out.json", JSON.stringify(result));
