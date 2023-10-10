import { CodeMirror } from './libs/editor/wisp.editor.bundle.js';
import { printErrors, executeSQL } from './driver.js';
export const consoleElement = document.getElementById('console');
export const tableContainer = document.getElementById('table-container');
export const editorContainer = document.getElementById('editor-container');
export const labelContainer = document.getElementById('labels-container');
export const emptyImage = document.getElementById('empty-image');
export const execButton = document.getElementById('exe');
export const keyButton = document.getElementById('key');
// export const copyButton = document.getElementById('copy-table');
export const editor = CodeMirror(editorContainer, {});
editor.setSize(window.innerWidth - 15, window.innerHeight);

window.addEventListener('resize', () =>
  tableContainer.innerHTML || emptyImage.style.display !== 'none'
    ? editor.setSize(window.innerWidth - 15, window.innerHeight / 2)
    : editor.setSize(window.innerWidth - 15, window.innerHeight)
);

document.addEventListener('keydown', e => {
  if (e.key.toLowerCase() === 's' && (e.ctrlKey || e.metaKey)) {
    e = e || window.event;
    e.preventDefault();
    e.stopPropagation();
    executeSQL(editor.getValue());
    const newurl =
      window.location.protocol +
      '//' +
      window.location.host +
      window.location.pathname +
      `?l=${encodeURIComponent(
        LZUTF8.compress(editor.getValue(), {
          inputEncoding: 'String',
          outputEncoding: 'Base64'
        })
      )}`;
    window.history.pushState({ path: newurl }, '', newurl);
  }
});

const initial = new URLSearchParams(location.search).get('l') ?? '';
if (initial) {
  try {
    editor.setValue(
      decodeURIComponent(
        LZUTF8.decompress(initial, {
          inputEncoding: 'Base64',
          outputEncoding: 'String'
        })
      )
    );
  } catch (e) {
    printErrors(e);
  }
}
// copyButton.addEventListener('click', copyTable  );
execButton.addEventListener('click', () => executeSQL(editor.getValue(), 1));
keyButton.addEventListener('click', () => executeSQL(editor.getValue(), 0));

// setTimeout(() => {
//   document.body.removeChild(document.getElementById('splash-screen'));
//   const db = window.localStorage.getItem('HyperLightDB');
// }, 1000);
