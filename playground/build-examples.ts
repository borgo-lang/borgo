import { marked } from "https://esm.sh/marked@4.0.18";
import { slugify } from "../compiler/test/runner.ts";

Deno.chdir(Deno.cwd() + "/playground");

type TourBlock = {
  title: string;
  description: string;
  code: string;
};

function markdownToTourblocks(content: string): Array<TourBlock> {
  let tourBlocks: Array<TourBlock> = [];
  let only: TourBlock | undefined;
  let title = "";
  let parsingHint = false;
  let description: Array<string> = [];

  marked.parse(content, {
    walkTokens(token) {
      if (token.type === "heading") {
        title = token.text;
        return;
      }

      if (token.type === "code" && !token.lang) {
        // code blocks within description
        description.push(marked.parse(token.raw));
        return;
      }

      if (token.type === "code") {
        const block = {
          slug: slugify(title),
          title,
          description: description.join("\n"),
          code: token.text,
        };
        description = [];
        title = "";

        if (token.lang === "rust-skip") {
          // Skip this block
          return;
        }

        if (token.lang === "rust-only") {
          only = block;
          return;
        }

        tourBlocks.push(block);
        return;
      }

      if (token.type === "hr") {
        parsingHint = !parsingHint;
        return;
      }

      if (token.type === "paragraph") {
        let html = marked.parse(token.raw);

        if (parsingHint) {
          html = `<blockquote class="hint">${html}</blockquote>`;
        }

        description.push(html);
        return;
      }

      if (token.type === "list") {
        description.push(marked.parse(token.raw));
      }
    },
  });

  if (only) tourBlocks = [only];

  return tourBlocks;
}

function decode(s: any) {
  return new TextDecoder().decode(s);
}

// --------------------
// Build examples
// --------------------

{
  const content = Deno.readTextFileSync("./examples.md");
  const tourBlocks = markdownToTourblocks(content);

  const result = [];

  for (const b of tourBlocks) {
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

    result.push({ ...b, code: output });
  }

  Deno.writeTextFileSync("./static/examples.out.json", JSON.stringify(result));
}

// --------------------
// Build home
// --------------------

{
  const tpl = Deno.readTextFileSync("./home.html");
  const readme = Deno.readTextFileSync("../README.md");

  const md = marked.parse(readme);

  const content = tpl.replace("[content]", md);
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
