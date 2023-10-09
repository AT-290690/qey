import { readFileSync, writeFileSync } from 'fs'
import { start } from 'repl'
import { compileToJs } from '../src/compiler.js'
import { run, stacktrace } from '../src/interpreter.js'
import { parse } from '../src/parser.js'
import { format } from '../src/formatter.js'
import fsExtension from '../lib/extensions/fs.js'
import dateExtension from '../lib/extensions/date.js'
// import wabt from 'wabt'
import { logError, removeNoCode, treeShake } from '../src/utils.js'
import STD from '../lib/baked/std.js'
import MATH from '../lib/baked/math.js'
import DS from '../lib/baked/ds.js'
const libraries = {
  std: STD,
  math: MATH,
  ds: DS,
}
const libs = [...libraries['std'], ...libraries['math'], ...libraries['ds']]
import { APPLY, TYPE, VALUE, WORD } from '../src/enums.js'
export default async () => {
  const [, , ...argv] = process.argv
  let file = '',
    path = '',
    Extensions = {},
    Helpers = {},
    Tops = [],
    env = {},
    destination = undefined,
    lib = 'std'
  while (argv.length) {
    const flag = argv.shift()?.toLowerCase()
    const value = argv.shift()
    if (!flag) throw new Error('No flag provided')
    switch (flag) {
      case '-m':
        writeFileSync(path, removeNoCode(file), 'utf-8')
        break
      case '-d':
        destination = value
        break
      case '-s':
        path = value
        file = readFileSync(value, 'utf-8')
        break
      case '-c':
        {
          const tree = parse(file)
          if (Array.isArray(tree)) {
            const { top, program, deps } = compileToJs(
              tree,
              Extensions,
              Helpers,
              Tops
            )
            const JavaScript = `${top}${treeShake(
              deps,
              JSON.parse(JSON.stringify(Object.values(libraries)))
            )}${program}`
            writeFileSync(
              destination ?? './playground/dist/main.js',
              JavaScript
            )
          }
        }
        break
      case '-e':
        {
          switch (value) {
            case 'fs':
              {
                Extensions = { ...Extensions, ...fsExtension.Extensions }
                Helpers = { ...Helpers, ...fsExtension.Helpers }
                Tops = [...Tops, ...fsExtension.Tops]
                env = { ...env, ...fsExtension.env }
              }
              break
            case 'date':
              {
                Extensions = { ...Extensions, ...dateExtension.Extensions }
                Helpers = { ...Helpers, ...dateExtension.Helpers }
                Tops = [...Tops, ...dateExtension.Tops]
                env = { ...env, ...dateExtension.env }
              }
              break
          }
        }
        break
      case '-p':
        try {
          run(parse(file), env)
        } catch (err) {
          logError(err.message)
        }
        break
      case '-r':
        try {
          run([...libs, ...parse(file)], env)
        } catch (err) {
          logError('Error')
          logError(err.message)
          console.log(
            ` \x1b[30m${[...stacktrace]
              .reverse()
              .filter(Boolean)
              .join('\n ')}\x1b[0m`
          )
        }
        break
      case '-trace':
        try {
          run([...libs, ...parse(file)], env)
        } catch (err) {
          console.log('\x1b[40m', err, '\x1b[0m')
          logError(err.message)
        }
        break
      case '-lib':
        lib = value
        break
      case '-doc':
        {
          const mods = []
          const parsed = libraries[lib].at(-1).at(-1).slice(1)
          parsed.pop()
          mods.push(
            parsed.filter(
              ([dec, name]) =>
                dec[TYPE] === APPLY &&
                dec[VALUE] === 'defun' &&
                name[TYPE] === WORD
            )
          )
          ;(value
            ? mods.flat(1).filter(([, x]) => x[VALUE].includes(value))
            : mods.flat(1)
          ).forEach(([, name, ...rest]) => {
            console.log(
              `(\x1b[33m${name[VALUE]}\x1b[36m ${rest
                .map((x) => x[VALUE])
                .join(' ')
                .trimRight()}\x1b[0m)`
            )
          })
        }
        break
      case '-format':
        const tree = parse(file)
        if (Array.isArray(tree)) {
          writeFileSync(path, format(tree), 'utf-8')
        }
        break
      case '-import':
        {
          const mods = []
          const parsed = libraries[lib].at(-1).at(-1).slice(1)
          parsed.pop()
          mods.push(
            parsed.filter(
              ([dec, name]) =>
                dec[TYPE] === APPLY &&
                dec[VALUE] === 'defun' &&
                name[TYPE] === WORD
            )
          )
          console.log(
            `\x1b[35m${(value
              ? mods.flat(1).filter(([, x]) => x[VALUE].includes(value))
              : mods.flat(1)
            )
              .map(([, name]) => {
                return `"${name[VALUE].trimRight()}"`
              })
              .join(' ')}\x1b[0m`
          )
        }
        break
      case '-repl':
        {
          let source = ''
          const inpColor = '\x1b[32m'
          const outColor = '\x1b[33m'
          const errColor = '\x1b[31m'
          console.log(inpColor)
          start({
            prompt: '',
            eval: (input) => {
              input = input.trim()
              if (!input || input[0] === ';') return
              try {
                let out = `${source}\n${file}\n(do ${input})`
                const result = run([...libs, ...parse(out)], env)
                if (typeof result === 'function') {
                  console.log(inpColor, `(λ)`)
                } else if (Array.isArray(result)) {
                  console.log(
                    outColor,
                    JSON.stringify(result, (_, value) => {
                      switch (typeof value) {
                        case 'bigint':
                          return Number(value)
                        case 'function':
                          return 'λ'
                        case 'undefined':
                        case 'symbol':
                          return 0
                        case 'boolean':
                          return +value
                        default:
                          return value
                      }
                    })
                      .replace(new RegExp(/\[/g), '(')
                      .replace(new RegExp(/\]/g), ')')
                      .replace(new RegExp(/\,/g), ' ')
                      .replace(new RegExp(/"λ"/g), 'λ'),
                    inpColor
                  )
                } else if (typeof result === 'string') {
                  console.log(outColor, `"${result}"`, inpColor)
                } else if (result == undefined) {
                  console.log(errColor, '(void)', inpColor)
                } else {
                  console.log(outColor, result, inpColor)
                }
                source = out
              } catch (err) {
                console.log(errColor, err.message, inpColor)
              }
            },
          })
        }
        break
      case '-help':
      case '-h':
      default:
        console.log(`
-------------------------------------
-help
-------------------------------------
-lib                      target lib
-------------------------------------
-doc              list lib functions
-------------------------------------
-import           log import for lib
-------------------------------------
-s                    prepare a file
-------------------------------------
-d               file to compile js
-------------------------------------
-c                    compile to js
-------------------------------------
-r                  interpret & run
-------------------------------------
-p      interpret & run with 0 deps
-------------------------------------
-m                      minify code 
-------------------------------------
-repl    start Read Eval Print Loop
-------------------------------------
`)
    }
  }
}
