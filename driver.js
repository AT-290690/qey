import { compileToJs } from './libs/node-lisper/src/compiler.js';
import { evaluate, run } from './libs/node-lisper/src/interpreter.js';
import { treeShake } from './libs/node-lisper/src/utils.js';
import { parse } from './libs/node-lisper/src/parser.js';
import std from './libs/node-lisper/lib/baked/std.js';
import math from './libs/node-lisper/lib/baked/math.js';
import ds from './libs/node-lisper/lib/baked/ds.js';
import { consoleElement, editor, emptyImage, tableContainer } from './main.js';
import { APPLY, TYPE, VALUE, WORD } from './libs/node-lisper/src/enums.js';
const libraries = {
  std,
  math,
  ds
};
const libs = [...libraries['std'], ...libraries['math'], ...libraries['ds']];
const ENV = {
  ['follow']: (args, env) => {
    const link = evaluate(args[0], env);
    if (!Array.isArray(link)) {
      throw new TypeError('Follow link argument must be a table');
    }
    window.open(link.at(1).at(0), '_blank');
  },
  ['link']: (args, env) => {
    const compressed = encodeURIComponent(
      LZUTF8.compress(editor.getValue(), {
        inputEncoding: 'String',
        outputEncoding: 'Base64'
      })
    );
    const destination =
      window.location.protocol +
      '//' +
      window.location.host +
      window.location.pathname;
    const link = `${destination}?l=${compressed}`;
    // const data = evaluate(args[0], env);
    return [['Link'], [link]];
  },
  ['compress']: (args, env) => {
    // const data = evaluate(args[0], env);
    const compressed = encodeURIComponent(
      LZUTF8.compress(editor.getValue(), {
        inputEncoding: 'String',
        outputEncoding: 'Base64'
      })
    );
    return [['Base64'], [compressed]];
  },
  ['decompress']: (args, env) => {
    // const data = evaluate(args[0], env);
    const decompressed = decodeURIComponent(
      LZUTF8.decompress(editor.getValue(), {
        inputEncoding: 'Base64',
        outputEncoding: 'String'
      })
    );
    return [['String'], [decompressed]];
  },
  ['copy']: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        'Invalid number of arguments to (copy) [1 required]'
      );
    const table = evaluate(args[0], env);
    if (!Array.isArray(table) || !Array.isArray(table[0])) {
      throw new TypeError('Argument of copy must be a table');
    }
    copyTable(table);
    return table;
  },
  ['download']: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        'Invalid number of arguments to (download) [2 required]'
      );
    const table = evaluate(args[0], env);
    if (!Array.isArray(table) || !Array.isArray(table[0])) {
      throw new TypeError('First argument of download must be a table');
    }
    const name = evaluate(args[1], env);
    if (typeof name !== 'string') {
      throw new TypeError('Second argument of download must be a string');
    }
    downloadCSVFile(table.map(x => x.join(',')).join('\n'), name);
    return table;
  },
  ['doc']: (args, env) => {
    if (args.length !== 1)
      throw new RangeError('Invalid number of arguments to (doc) [1 required]');
    const name = evaluate(args[0], env);
    // if (typeof name !== 'string') {
    //   throw new TypeError('Second argument of download must be a string');
    // }
    const mods = [];
    const parsed = libraries[name].at(-1).at(-1).slice(1);
    parsed.pop();
    mods.push(
      parsed.filter(
        ([dec, name]) =>
          dec[TYPE] === APPLY && dec[VALUE] === 'defun' && name[TYPE] === WORD
      )
    );
    // (value
    //   ? mods.flat(1).filter(([, x]) => x[VALUE].includes(value))
    //   : mods.flat(1)
    return mods.flat(1).reduce(
      (acc, [, name, ...rest]) => {
        acc.push([
          name[VALUE],
          rest
            .map(x => x[VALUE])
            .join(' ')
            .trimRight(),
          rest.length
        ]);
        return acc;
      },
      [['function', 'arguments', 'n']]
    );
  }
};

export const printErrors = errors => {
  // consoleElement.classList.remove('info_line');
  // consoleElement.classList.add('error_line');
  consoleElement.textContent = errors;
  consoleElement.style.display = 'block';
};
const compileAndEval = source => {
  const tree = parse(source);
  if (Array.isArray(tree)) {
    const { top, program, deps } = compileToJs(tree);

    const JavaScript = `${top}${treeShake(
      deps,
      JSON.parse(JSON.stringify(Object.values(libraries)))
    )}${program}`;
    return eval(JavaScript);
  }
};

const execute = (source, compile = 0) => {
  try {
    consoleElement.style.display = 'none';
    // consoleElement.classList.remove('error_line');
    if (!source.trim()) return;
    const result = compile
      ? compileAndEval(source)
      : run([...libs, ...parse(source)], ENV);
    // droneButton.classList.remove('shake');
    // errorIcon.style.visibility = 'hidden';
    // droneIntel(execIcon);
    return result;
  } catch (err) {
    printErrors(err);
  }
};
export const copyTable = ([cols, ...rows]) => {
  // consoleElement.classList.add('info_line');
  // consoleElement.classList.remove('error_line');
  if (!cols.length && !rows.length) {
    navigator.clipboard.writeText('No results!');
    return (consoleElement.textContent = 'No results to copy!');
  } else {
    const date = new Date();
    const formatted =
      date.getDate() +
      '-' +
      (date.getMonth() + 1) +
      '-' +
      date.getFullYear() +
      ' ' +
      date.getHours() +
      ':' +
      date.getMinutes();
    navigator.clipboard.writeText(
      `\n\n${cols.join(' ')}\n\n${rows.join('\n')}\n\n ${formatted}`
    );
    consoleElement.style.display = 'block';
    consoleElement.textContent = 'Copied table to clipboard!';
    setTimeout(() => {
      consoleElement.style.display = 'none';
      consoleElement.textContent = '';
    }, 3000);
  }
};
const isIterable = obj =>
  obj != null && typeof obj[Symbol.iterator] === 'function';
export const executeSQL = (sql = editor.getValue(), compile = 0) => {
  // editor.setSize(window.innerWidth, window.innerHeight);
  tableContainer.style.display = 'none';
  tableContainer.innerHTML = '';
  emptyImage.style.display = 'none';
  consoleElement.textContent = '';
  // consoleElement.classList.add('info_line');
  // consoleElement.classList.remove('error_line');

  const table = document.createElement('table');
  let tr = table.insertRow(-1);
  let Rows = [[], []];

  try {
    Rows = execute(sql, compile);
    if (!isIterable(Rows)) return [];
    const [cols, ...rows] = Rows;
    if (!Array.isArray(cols) || !rows.length) {
      emptyImage.style.display = 'block';
      return editor.setSize(window.innerWidth, window.innerHeight / 2);
    }

    cols.forEach(key => {
      const th = document.createElement('th');
      th.innerHTML = key;
      tr.appendChild(th);
    });

    for (let i = 0; i < rows.length; i++) {
      tr = table.insertRow(-1);
      for (let j = 0; j < cols.length; j++) {
        // console.log(rows[i][j]);
        const tabCell = tr.insertCell(-1);
        tabCell.innerHTML = rows[i][j];
      }
    }
    tableContainer.appendChild(table);
    tableContainer.style.display = 'block';
    editor.setSize(window.innerWidth - 15, window.innerHeight / 2);
  } catch (err) {
    printErrors(err);
  }
};
export const htmlToCSV = html => {
  const data = [];
  const rows = document.querySelectorAll('table tr');

  for (let i = 0; i < rows.length; i++) {
    const row = [],
      cols = rows[i].querySelectorAll('td, th');

    for (let j = 0; j < cols.length; j++) {
      row.push(cols[j].innerText);
    }

    data.push(row.join(','));
  }
  return data;
};

export const downloadCSVFile = (csv, filename) => {
  const csvFile = new Blob([csv], { type: 'text/csv' });
  const downloadLink = document.createElement('a');
  downloadLink.download = filename;
  downloadLink.href = window.URL.createObjectURL(csvFile);
  downloadLink.style.display = 'none';
  document.body.appendChild(downloadLink);
  downloadLink.click();
};
