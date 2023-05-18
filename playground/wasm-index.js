import { basicSetup, EditorView } from "codemirror";
import { keymap } from "@codemirror/view";
import { rust } from "@codemirror/lang-rust";

const Borgo = {
  compile_wasm: function () {
    throw new Error("compile_wasm not initialized");
  },
};

window.Borgo = Borgo;

let output = [];
let EXAMPLES;
let STD_SOURCE;

const elements = {
  output: document.getElementById("output"),
  errors: document.getElementById("errors"),
  examples: document.getElementById("examples"),
  links: () => document.querySelectorAll("#examples a"),
  section_title: document.getElementById("section-title"),
  section_description: document.getElementById("section-description"),
};

function updateLog(s) {
  output.push(s);
  elements.output.textContent = output.join("\n");
}

function resetLogs() {
  output = [];
  elements.output.textContent = "";
  elements.errors.textContent = "";
}

function updateError(err) {
  elements.errors.textContent = err;
}

async function initStaticData() {
  EXAMPLES = await (fetch("/examples.out.json").then((res) => res.json()));
  STD_SOURCE = await (fetch("/std/core.brg").then((res) => res.text()));
}

async function initExamples() {
  EXAMPLES.forEach((e) => {
    const link =
      `<a href="#${e.slug}" onClick="Borgo.selectExample(${e.slug})">${e.title}</a>`;
    elements.examples.innerHTML += link;
  });
}

async function callGoPlayground(code) {
  const res = await fetch(
    "https://go-compiler.borgo.workers.dev/",
    {
      "credentials": "omit",
      "headers": {
        "Content-Type": "application/json",
      },
      "body": JSON.stringify({ code }),
      "method": "POST",
      "mode": "cors",
    },
  );

  return await res.json();
}

function aggregateProject(project) {
  let source = [];
  let imports = {};

  for (const pkg of Object.values(project)) {
    for (const [path, name] of Object.entries(pkg.imports)) {
      imports[path] = name ?? "";
    }

    source.push(pkg.source);
  }

  const rendered_imports = Object.entries(imports).map(([path, name]) => {
    return `import ${name} "${path}"`;
  });

  return ["package main", ...rendered_imports, ...source].join(
    "\n",
  );
}

function initRouting() {
  function refresh() {
    const hash = window.location.hash ||
      elements.links()[0].getAttribute("href");

    Borgo.selectExample(hash.substr(1));
  }

  window.addEventListener("hashchange", refresh);
  window.addEventListener("popstate", refresh);
  refresh();
}

Borgo.selectExample = function (slug) {
  const e = EXAMPLES.find((e) => e.slug == slug);
  const state = Borgo.view.state;

  const update = state.update({
    changes: { from: 0, to: state.doc.length, insert: e.code },
  });

  // Update editor content
  Borgo.view.update([update]);
  resetLogs();

  elements.section_title.textContent = e.title;
  elements.section_description.innerHTML = e.description;

  // Update active link in sidebar
  const links = elements.links();
  links.forEach((e) => {
    e.classList.remove("active");

    if (e.getAttribute("href").substr(1) == slug) {
      e.classList.add("active");
    }
  });
};

Borgo.main = async function () {
  resetLogs();

  await initStaticData();
  initExamples();

  const editor = document.getElementById("editor");
  const initial_content = editor.getElementsByTagName("textarea")[0].value;

  let view = new EditorView({
    doc: initial_content,
    extensions: [
      basicSetup,
      rust({}),

      // Keybinding to run compiler
      keymap.of([{
        key: "Shift-Enter",
        preventDefault: true,
        run: () => {
          compile();
        },
      }]),
    ],
  });

  editor.insertAdjacentElement("afterend", view.dom);
  editor.remove();

  // Set global editor
  Borgo.view = view;

  Borgo.log = function (s) {
    updateLog(s);
  };

  // --------------------

  Borgo.compile = async function () {
    resetLogs();

    const user_source = view.state.doc.toString();

    try {
      const compiler_res = Borgo.compile_wasm(user_source, STD_SOURCE);
      const project = JSON.parse(compiler_res);

      updateLog("Waiting for server...");
      const full_source = aggregateProject(project);
      const result = await (callGoPlayground(full_source));

      if (result) {
        const events = (result.Events ?? []).map((e) => e.Message).join("\n");

        resetLogs();
        updateLog(result.Errors + events);
        updateLog("\nProgram exited.");
      }

      Borgo.project = project;
    } catch (e) {
      updateError(e);
    }
  };

  // Init router and select first example
  initRouting();
};
