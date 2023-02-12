import { basicSetup, EditorView } from "codemirror";
import { hoverTooltip, keymap } from "@codemirror/view";
import { rust } from "@codemirror/lang-rust";

const Borgo = {};

window.Borgo = Borgo;

let output = [];

const elements = {
  output: document.getElementById("output"),
  errors: document.getElementById("errors"),
  examples: document.getElementById("examples"),
};

function updateLog(s) {
  output.push(s);
  elements.output.textContent = output.join("\n");
}

function resetLogs() {
  output = [];
  elements.output.textContent = "no output";
  elements.errors.textContent = "no errors";
}

function updateError(err) {
  elements.errors.textContent = err;
}

let EXAMPLES;
let STD_SOURCE;

async function initStaticData() {
  EXAMPLES = await (fetch("examples.out.json").then((res) => res.json()));
  STD_SOURCE = await (fetch("std/core.brg").then((res) => res.text()));
}

initStaticData();

async function initExamples() {
  EXAMPLES.forEach((e, index) => {
    const link =
      `<a href="#" onClick="Borgo.selectExample(${index})">${e.title}</a>`;
    elements.examples.innerHTML += link;
  });
}

Borgo.selectExample = function (index) {
  const e = EXAMPLES[index];
  const state = Borgo.view.state;

  const update = state.update({
    changes: { from: 0, to: state.doc.length, insert: e.code },
  });

  // Update editor content
  Borgo.view.update([update]);

  resetLogs();
};

Borgo.main = async function () {
  resetLogs();
  initExamples();

  const textarea = document.getElementById("code");
  const initial_content = textarea.value;

  // Hover Tooltip
  const wordHover = hoverTooltip((view, pos, side) => {
    const line = view.state.doc.lineAt(pos);

    // TODO put compilation in standalone fn and cache result
    const user_source = view.state.doc.toString();
    const project = Borgo.compile(user_source, STD_SOURCE);
    const hover = Borgo.on_hover(project, line.number, pos - line.from);

    return {
      pos,
      end: pos,
      above: true,
      create(view) {
        let dom = document.createElement("div");
        dom.textContent = hover;
        return { dom };
      },
    };
  });

  // Create a CodeMirror editor with the textarea's contents
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
      // Disable tooltips for now, as they're not very reliable
      //  wordHover,
    ],
  });

  textarea.insertAdjacentElement("afterend", view.dom);
  textarea.remove();

  // Set global editor
  Borgo.view = view;

  Borgo.log = function (s) {
    updateLog(s);
  };

  // --------------------

  window.compile = function () {
    const user_source = view.state.doc.toString();
    resetLogs();

    try {
      const project = Borgo.compile(user_source, STD_SOURCE);
      const result = runProject(project);

      if (result) {
        updateLog(result);
      }

      Borgo.project = project;
    } catch (e) {
      updateError(e);
    }
  };
};
