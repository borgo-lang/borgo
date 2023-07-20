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

let READY = { data: false, compiler: false };
let STD_SOURCE;

const elements = {
  output: document.getElementById("output"),
  errors: document.getElementById("errors"),
  examples: Array.from(document.getElementById("examples").children),
  content: Array.from(document.getElementById("content").children),
};

// Build up index of examples source code
const EXAMPLES = elements.content.reduce((acc, item) => {
  const slug = item.dataset.slug;

  const code = JSON.parse(
    item.querySelector("pre[data-example]").dataset.example,
  );

  acc[slug] = { code };
  return acc;
}, {});

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
  STD_SOURCE = await (fetch("/std.out.json").then((res) => res.json()));
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

function aggregateProject(emitted_files) {
  let source = [];
  let imports = {};

  for (const file of emitted_files) {
    for (const [path, name] of file.imports) {
      imports[path] = name ?? "";
    }

    source.push(file.source);
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
      elements.examples[0].getAttribute("href");

    Borgo.selectExample(hash.substring(1));
  }

  window.addEventListener("hashchange", refresh);
  window.addEventListener("popstate", refresh);
  refresh();
}

Borgo.selectExample = function (slug) {
  const e = EXAMPLES[slug];
  const state = Borgo.view.state;

  const update = state.update({
    changes: { from: 0, to: state.doc.length, insert: e.code },
  });

  // Update editor content
  Borgo.view.update([update]);
  resetLogs();

  // Set section as visible
  elements.content.forEach((e) => {
    e.classList.remove("active");

    if (e.dataset.slug == slug) {
      e.classList.add("active");
    }
  });

  // Update active link in sidebar
  elements.examples.forEach((e) => {
    e.classList.remove("active");

    if (e.getAttribute("href").substring(1) == slug) {
      e.classList.add("active");
    }
  });
};

Borgo.compiler_ready = function () {
  READY.compiler = true;
  resetLogs();
};

Borgo.main = async function () {
  initStaticData()
    .then(() => READY.data = true);

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

    if (!READY.compiler || !READY.data) {
      updateLog("Compiler still loading :(");
      return;
    }

    const user_source = view.state.doc.toString();

    const std = { packages: STD_SOURCE };

    try {
      const project = Borgo.compile_wasm(user_source, std);

      if (project.error) {
        updateError(project.error);
        return;
      }

      updateLog("Waiting for server...");
      const full_source = aggregateProject(project.emitted);
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
