import { decode, markdownToCodeblocks } from "../compiler/test/runner.ts";
import { marked } from "https://esm.sh/marked@4.0.18";

// --------------------
// Build examples
// --------------------

{
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
}

// --------------------
// Build home
// --------------------

{
  const tpl = Deno.readTextFileSync("./home.html");
  const readme = Deno.readTextFileSync("../README.md");

  let intro = marked.parse(readme);
  const needle = `<h2 id="goals"`;
  intro = intro.split(needle)[1];
  intro = intro.split(`<h2 id="tour"`)[0];
  intro = needle + intro;

  const content = tpl.replace("[content]", intro);
  Deno.writeTextFileSync("./static/index.html", content);
}

// --------------------
// Build playground
// --------------------
{
  try {
    Deno.mkdirSync("./static/playground");
  } catch (e) {}

  Deno.copyFileSync("./playground.html", "./static/playground/index.html");
}
