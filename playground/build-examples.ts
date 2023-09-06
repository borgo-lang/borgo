import { marked } from "https://esm.sh/marked@4.0.18";
import { slugify } from "../compiler/test/runner.ts";
import { walkSync } from "https://deno.land/std/fs/mod.ts";
import { dirname } from "https://deno.land/std/path/mod.ts";
import { Eta } from "https://deno.land/x/eta@v3.0.3/src/index.ts";

Deno.chdir(Deno.cwd() + "/playground");

let EXAMPLES;

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

  EXAMPLES = tourBlocks;
  Deno.writeTextFileSync("./examples.out.json", JSON.stringify(tourBlocks));
}

// --------------------
// Build playground
// --------------------
{
  const eta = new Eta({ views: "." });
  const html = eta.render("./playground", { examples: EXAMPLES });
  Deno.writeTextFileSync("./static/index.html", html);
}

// --------------------
// Build std
// --------------------

{
  const oldCwd = Deno.cwd();

  Deno.chdir(Deno.cwd() + "/../std");

  // Walk std/ and create a single json file with all packages
  const packages = {};

  for (const entry of walkSync(".", { includeDirs: false })) {
    const folder = dirname(entry.path);

    if (folder == ".") {
      continue;
    }

    packages[folder] = packages[folder] || {};
    packages[folder][entry.name] = Deno.readTextFileSync(entry.path);
  }

  Deno.chdir(oldCwd);
  Deno.writeTextFileSync(
    "./static/std.out.json",
    JSON.stringify(packages),
  );
}

console.log("DONE");
