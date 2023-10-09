import { APPLY, ATOM, PLACEHOLDER, TOKENS, TYPE, VALUE, WORD } from './enums.js'
export const earMuffsToLodashes = (name) => name.replace(new RegExp(/\*/g), '_')
export const dotNamesToEmpty = (name) => name.replace(new RegExp(/\./g), '')
export const colonNamesTo$ = (name) => name.replace(new RegExp(/\:/g), '$')
export const commaToLodash = (name) => name.replace(new RegExp(/\,/g), '_')
export const questionMarkToLodash = (name) =>
  name.replace(new RegExp(/\?/g), 'Pre')
export const exclamationMarkMarkToLodash = (name) =>
  name.replace(new RegExp(/\!/g), 'Mut')
export const toCamelCase = (name) => {
  let out = name[0]
  for (let i = 1; i < name.length; ++i) {
    const current = name[i],
      prev = name[i - 1]
    if (current === '-') continue
    else if (prev === '-') {
      out += current.toUpperCase()
    } else out += current
  }
  return out
}
export const deepRename = (name, newName, tree) => {
  if (Array.isArray(tree))
    for (const branch of tree) {
      if (branch[VALUE] === name) branch[VALUE] = `()=>${newName}`
      deepRename(name, newName, branch)
    }
}
export const lispToJavaScriptVariableName = (name) =>
  toCamelCase(
    dotNamesToEmpty(
      colonNamesTo$(
        exclamationMarkMarkToLodash(
          questionMarkToLodash(commaToLodash(earMuffsToLodashes(name)))
        )
      )
    )
  )

const Extensions = {}
const Helpers = {
  log: {
    source: `var log = (msg) => { console.log(msg); return msg }`,
  },
  _identity: {
    source: `_identity = i => { return i }`,
  },
  tco: {
    source: `tco = fn => (...args) => {
      let result = fn(...args)
      while (typeof result === 'function') result = result()
      return result
    }`,
  },
  'regex-match': {
    source: `_regExpMatch = (string, regex) => {
      const match = string.match(new RegExp(regex, 'g'))
      return match == undefined ? [] : [...match]
    }`,
  },
  'regex-replace': {
    source: `_regExpReplace = (string, a, b) => string.replace(new RegExp(a, 'g'), b)`,
  },
  atom: {
    source: `_isAtom = (value) => typeof value === 'number' ||  typeof value === 'bigint' || typeof value === 'string'`,
  },
  set: {
    source: `_set = (array, index, value) => { 
      if (index < 0) {
       const target = array.length + index
       while (array.length !== target) array.pop()
      } else array[index] = value; 
      return array 
  }`,
  },
  error: {
    source: `_error = (error) => { 
      throw new Error(error)
  }`,
  },
  cast: {
    source: `_cast = (type, value) => {
    switch (type) {
      case 'Number':
         return Number(value)
      case 'Integer':
         return BigInt(value)
      case 'String':
         return value.toString()
      case 'Array':
        return typeof value === 'number' || typeof value === 'bigint' ? [...Number(value).toString()].map(Number) : [...value]
      case 'Bit':
         return parseInt(value, 2)
      case 'Boolean':
          return +!!value
      case 'Function':
          return () => value
       default:
         return 0
      }
    }`,
  },
}
const handleBoolean = (source) => `+${source}`
const semiColumnEdgeCases = new Set([
  ';)',
  ';-',
  ';+',
  ';*',
  ';%',
  ';&',
  ';/',
  ';:',
  ';.',
  ';=',
  ';<',
  ';>',
  ';|',
  ';,',
  ';?',
  ',,',
  ';;',
  ';]',
])

const parse = (Arguments, Variables, Functions) =>
  Arguments.map((x) => compile(x, Variables, Functions))
const parseArgs = (Arguments, Variables, Functions, separator = ',') =>
  parse(Arguments, Variables, Functions).join(separator)
const compile = (tree, Variables, Functions) => {
  if (!tree) return ''
  const [first, ...Arguments] = Array.isArray(tree) ? tree : [tree]
  if (first == undefined) return '[];'
  const token = first[VALUE]
  if (first[TYPE] === APPLY) {
    switch (token) {
      case TOKENS.BLOCK: {
        if (Arguments.length > 1) {
          return `(${Arguments.map((x) =>
            (compile(x, Variables, Functions) ?? '').toString().trimStart()
          )
            .filter(Boolean)
            .join(',')});`
        } else {
          const res = compile(Arguments[0], Variables, Functions)
          return res !== undefined ? res.toString().trim() : ''
        }
      }
      case TOKENS.CALL_FUNCTION: {
        const [first, ...rest] = Arguments
        const apply = compile(first, Variables, Functions)
        return `${
          apply[apply.length - 1] === ';'
            ? apply.substring(0, apply.length - 1)
            : apply
        }(${parseArgs(rest, Variables, Functions)})`
      }
      case TOKENS.DESTRUCTURING_ASSIGMENT: {
        let out = `((_=${compile(Arguments.pop(), Variables, Functions)},`
        Variables.add('_')
        const _rest = Arguments.pop()
        const len = Arguments.length
        for (let i = 0; i < len; ++i) {
          const NAME = Arguments[i][VALUE]
          if (NAME !== PLACEHOLDER) {
            const name = lispToJavaScriptVariableName(NAME)
            Variables.add(name)
            out += `${name}=_.at(${i})${i !== len - 1 ? ',' : ''}`
          } else {
            out += i !== len - 1 ? ',' : ''
          }
        }
        if (_rest[VALUE] !== PLACEHOLDER) {
          const rest = lispToJavaScriptVariableName(_rest[VALUE])
          Variables.add(rest)
          out += `,${rest}=_.slice(${len})), _);`
        } else out += `), _);`
        return out
      }
      case TOKENS.DEFINE_CONSTANT:
      case TOKENS.DEFINE_VARIABLE: {
        let name,
          out = '(('
        for (let i = 0, len = Arguments.length; i < len; ++i) {
          const arg = Arguments[i]
          if (i % 2 === 0 && arg[TYPE] === WORD) {
            name = lispToJavaScriptVariableName(arg[VALUE])
            Variables.add(name)
          } else
            out += `${name}=${compile(arg, Variables, Functions)}${
              i !== len - 1 ? ',' : ''
            }`
        }
        out += `),${name});`
        return out
      }
      case TOKENS.SET_VARIABLE:
      case TOKENS.SET_BOOLEAN: {
        const res = compile(Arguments[1], Variables, Functions)
        const arg = Arguments[0]
        if (arg[TYPE] === WORD) {
          const name = lispToJavaScriptVariableName(arg[VALUE])
          return `((${name}=${res}),${name});`
        }
        return ''
      }
      case TOKENS.FROM_CHAR_CODE:
        return `(String.fromCharCode(${compile(
          Arguments[0],
          Variables,
          Functions
        )}));`
      case TOKENS.CHAR_CODE_AT:
        return `((${compile(
          Arguments[0],
          Variables,
          Functions
        )}).charCodeAt(${compile(Arguments[1], Variables, Functions)}));`
      case TOKENS.MAKE_STRING:
        return `(String.fromCharCode(...${compile(
          Arguments[0],
          Variables,
          Functions
        )}));`
      case TOKENS.REGEX_MATCH:
        return `_regExpMatch(${parseArgs(Arguments, Variables, Functions)});`
      case TOKENS.REGEX_REPLACE:
        return `_regExpReplace(${parseArgs(Arguments, Variables, Functions)});`
      case TOKENS.IS_STRING:
        return handleBoolean(
          `(typeof(${compile(Arguments[0], Variables, Functions)})==='string');`
        )
      case TOKENS.IS_NUMBER:
        return handleBoolean(
          `(typeof(${compile(Arguments[0], Variables, Functions)})==='number');`
        )
      case TOKENS.IS_INTEGER:
        return handleBoolean(
          `(typeof(${compile(Arguments[0], Variables, Functions)})==='bigint');`
        )
      case TOKENS.IS_FUNCTION:
        return `(typeof(${compile(
          Arguments[0],
          Variables,
          Functions
        )})==='function');`
      case TOKENS.IS_ARRAY:
        return `(Array.isArray(${compile(
          Arguments[0],
          Variables,
          Functions
        )}));`
      case TOKENS.NUMBER_TYPE:
        return '0'
      case TOKENS.INTEGER_TYPE:
        return '0n'
      case TOKENS.BOOLEAN_TYPE:
        return '1'
      case TOKENS.STRING_TYPE:
        return '""'
      case TOKENS.SHORT_ARRAY:
        return `[${parseArgs(Arguments, Variables, Functions)}];`
      case TOKENS.ARRAY_TYPE:
        return Arguments.length === 2 &&
          Arguments[1][TYPE] === WORD &&
          Arguments[1][VALUE] === 'length'
          ? `(new Array(${compile(
              Arguments[0],
              Variables,
              Functions
            )}).fill(0))`
          : `[${parseArgs(Arguments, Variables, Functions)}];`
      case TOKENS.FUNCTION_TYPE:
        return '(()=>{});'
      case TOKENS.ARRAY_OR_STRING_LENGTH:
        return `(${compile(Arguments[0], Variables, Functions)}).length`
      case TOKENS.ATOM:
        return handleBoolean(
          `_isAtom(${compile(Arguments[0], Variables, Functions)});`
        )
      case TOKENS.FIRST_ARRAY:
        return `${compile(Arguments[0], Variables, Functions)}.at(0);`
      case TOKENS.REST_ARRAY:
        return `${compile(Arguments[0], Variables, Functions)}.slice(1);`
      case TOKENS.GET_ARRAY:
        return `${compile(Arguments[0], Variables, Functions)}.at(${compile(
          Arguments[1],
          Variables,
          Functions
        )});`
      case TOKENS.SET_ARRAY:
        return `_set(${parseArgs(Arguments, Variables, Functions)});`
      case TOKENS.ANONYMOUS_FUNCTION: {
        const functionArgs = Arguments
        const body = Arguments.pop()
        const Variables = new Set()
        const evaluatedBody = compile(body, Variables, Functions)
        const vars = Variables.size ? `var ${[...Variables].join(',')};` : ''
        return `((${parseArgs(
          functionArgs.map((node, index) =>
            node[VALUE] === PLACEHOLDER
              ? { [TYPE]: node[TYPE], [VALUE]: `_${index}` }
              : { [TYPE]: node[TYPE], [VALUE]: node[VALUE] }
          ),
          Variables
        )})=>{${vars}return ${evaluatedBody.toString().trimStart()}});`
      }
      case TOKENS.VARIADIC_FUNCTION: {
        const body = Arguments.pop()
        const Variables = new Set()
        const evaluatedBody = compile(body, Variables, Functions)
        const vars = Variables.size ? `var ${[...Variables].join(',')};` : ''
        return `((...${compile(
          Arguments[0],
          Variables
        )})=>{${vars}return ${evaluatedBody.toString().trimStart()}});`
      }
      case TOKENS.TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION: {
        let name,
          newName,
          out = '(('
        const arg = Arguments[1]
        name = lispToJavaScriptVariableName(arg[VALUE])
        newName = `rec_${performance.now().toString().replace('.', 7)}`
        Variables.add(name)
        Variables.add(newName)
        const functionArgs = Arguments.slice(2)
        const body = functionArgs.pop()
        const FunctionVariables = new Set()
        deepRename(arg[VALUE], newName, body)
        const evaluatedBody = compile(body, FunctionVariables, Functions)
        const vars = FunctionVariables.size
          ? `var ${[...FunctionVariables].join(',')};`
          : ''
        out += `${name}=(tco(${newName}=(${parseArgs(
          functionArgs,
          Variables
        )})=>{${vars}return ${evaluatedBody.toString().trimStart()}};`
        out += `, ${newName}))), ${name});`
        return out
      }
      case TOKENS.DEFINE_FUNCTION: {
        let name,
          out = '(('
        const arg = Arguments[0]
        name = lispToJavaScriptVariableName(arg[VALUE])
        Variables.add(name)
        const functionArgs = Arguments.slice(1)
        const body = functionArgs.pop()
        const FunctionVariables = new Set()
        const evaluatedBody = compile(body, FunctionVariables, Functions)
        const vars = FunctionVariables.size
          ? `var ${[...FunctionVariables].join(',')};`
          : ''
        out += `${name}=(${parseArgs(
          functionArgs.map((node, index) =>
            node[VALUE] === PLACEHOLDER
              ? { [TYPE]: node[TYPE], [VALUE]: `_${index}` }
              : { [TYPE]: node[TYPE], [VALUE]: node[VALUE] }
          ),
          Variables
        )})=>{${vars}return ${evaluatedBody.toString().trimStart()}};`
        out += `),${name});`
        return out
      }
      case TOKENS.AND:
        return `(${parseArgs(Arguments, Variables, Functions, '&&')});`
      case TOKENS.OR:
        return `((${parseArgs(Arguments, Variables, Functions, '||')}) || 0);`
      case TOKENS.STRING_CONCATENATION:
        return '(' + parseArgs(Arguments, Variables, Functions, '+') + ');'
      case TOKENS.EQUAL:
        return handleBoolean(
          `(${parseArgs(Arguments, Variables, Functions, '===')});`
        )
      case TOKENS.GREATHER_THAN_OR_EQUAL:
      case TOKENS.LESS_THAN_OR_EQUAL:
      case TOKENS.GREATHER_THAN:
      case TOKENS.LESS_THAN:
        return handleBoolean(
          `(${parseArgs(Arguments, Variables, Functions, token)});`
        )
      case TOKENS.SUBTRACTION:
        return Arguments.length === 1
          ? `(-${compile(Arguments[0], Variables, Functions)});`
          : `(${parse(Arguments, Variables, Functions)
              // Add space so it doesn't consider it 2--1 but 2- -1
              .map((x) => (typeof x === 'number' && x < 0 ? ` ${x}` : x))
              .join(token)});`
      case TOKENS.MULTIPLICATION:
        return Arguments.length
          ? `(${parseArgs(Arguments, Variables, Functions, token)});`
          : `(1);`
      case TOKENS.DIVISION:
        return Arguments.length
          ? Arguments.length === 1
            ? `(1/${compile(Arguments[0], Variables, Functions)});`
            : `(${parseArgs(Arguments, Variables, Functions, token)});`
          : `(0);`
      case TOKENS.ADDITION:
      case TOKENS.BITWISE_AND:
      case TOKENS.BITWISE_OR:
      case TOKENS.BITWISE_XOR:
      case TOKENS.BITWISE_LEFT_SHIFT:
      case TOKENS.BITWISE_RIGHT_SHIFT:
      case TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT:
        return `(${parseArgs(Arguments, Variables, Functions, token)});`
      case TOKENS.REMAINDER_OF_DIVISION:
        return `(${compile(Arguments[0], Variables, Functions)}%${compile(
          Arguments[1],
          Variables,
          Functions
        )});`
      case TOKENS.BIT_TYPE:
        return `(${compile(
          Arguments[0],
          Variables,
          Functions
        )}>>>0).toString(2)`
      case TOKENS.BITWISE_NOT:
        return `~(${compile(Arguments[0], Variables, Functions)})`
      case TOKENS.NOT:
        return `(${handleBoolean(
          `!${compile(Arguments[0], Variables, Functions)}`
        )})`
      case TOKENS.IF: {
        return `(${compile(Arguments[0], Variables, Functions)}?${compile(
          Arguments[1],
          Variables,
          Functions
        )}:${
          Arguments.length === 3
            ? compile(Arguments[2], Variables, Functions)
            : 0
        });`
      }
      case TOKENS.WHEN: {
        return `(${compile(Arguments[0], Variables, Functions)}?${compile(
          Arguments[1],
          Variables,
          Functions
        )}:0);`
      }
      case TOKENS.UNLESS: {
        return `(${compile(Arguments[0], Variables, Functions)}?${
          Arguments.length === 3
            ? compile(Arguments[2], Variables, Functions)
            : 0
        }:${compile(Arguments[1], Variables, Functions)});`
      }
      case TOKENS.OTHERWISE: {
        return `(${compile(Arguments[0], Variables, Functions)}?0:${compile(
          Arguments[1],
          Variables,
          Functions
        )});`
      }
      case TOKENS.CONDITION: {
        let out = '('
        for (let i = 0; i < Arguments.length; i += 2)
          out += `${compile(Arguments[i], Variables, Functions)}?${compile(
            Arguments[i + 1],
            Variables,
            Functions
          )}:`
        out += '0);'
        return out
      }
      case TOKENS.CAST_TYPE:
        return `_cast("${Arguments[1][VALUE]}", ${compile(
          Arguments[0],
          Variables,
          Functions
        )})`

      case TOKENS.PIPE: {
        let inp = Arguments[0]
        for (let i = 1; i < Arguments.length; ++i)
          inp = [Arguments[i].shift(), inp, ...Arguments[i]]
        return compile(inp, Variables, Functions)
      }
      case TOKENS.SLEEP: {
        return `setTimeout(${compile(
          Arguments[1],
          Variables,
          Functions
        )},${compile(Arguments[0], Variables, Functions)});`
      }
      case TOKENS.THROW_ERROR: {
        return `_error(${compile(Arguments[0], Variables, Functions)})`
      }
      case TOKENS.IMPORT:
        {
          const [module, ...functions] = Arguments.map((x) =>
            compile(x, Variables, Functions)
          )
          Functions.set(
            module,
            functions.map((fn) => {
              const name = lispToJavaScriptVariableName(
                fn.substring(1, fn.length - 1)
              )
              Variables.add(name)
              return name
            })
          )
        }
        break
      case TOKENS.IMMUTABLE_FUNCTION: {
        const [first, ...rest] = Arguments
        return compile(
          [{ [TYPE]: APPLY, [VALUE]: first[VALUE] }, ...rest],
          Variables,
          Functions
        )
      }

      case TOKENS.IDENTITY:
      case TOKENS.DEBUG:
      case TOKENS.ABORT:
      case TOKENS.NOT_COMPILED_BLOCK:
      case TOKENS.DEFINE_TYPE:
      case TOKENS.OR_TYPE:
      case TOKENS.AND_TYPE:
      case TOKENS.LAMBDA_TYPE:
        return ''
      default: {
        const camleCasedToken = lispToJavaScriptVariableName(token)
        if (camleCasedToken in Extensions)
          return `${Extensions[camleCasedToken](
            parseArgs(Arguments, Variables, Functions)
          )}`
        else
          return `${camleCasedToken}(${parseArgs(
            Arguments,
            Variables,
            Functions
          )});`
      }
    }
  } else if (first[TYPE] === ATOM)
    return typeof first[VALUE] === 'string'
      ? `\`${first[VALUE]}\``
      : first[VALUE]
  else if (first[TYPE] === WORD) return lispToJavaScriptVariableName(token)
}

export const compileToJs = (AST, extensions = {}, helpers = {}, tops = []) => {
  for (const ext in extensions)
    Extensions[lispToJavaScriptVariableName(ext)] = extensions[ext]
  for (const hlp in helpers)
    Helpers[lispToJavaScriptVariableName(hlp)] = helpers[hlp]
  const Variables = new Set()
  const Functions = new Map()
  const raw = AST.map((tree) => compile(tree, Variables, Functions))
    .filter(Boolean)
    .join('\n')
  let program = ''
  for (let i = 0; i < raw.length; ++i) {
    const current = raw[i]
    const next = raw[i + 1]
    if (!semiColumnEdgeCases.has(current + next)) program += current
  }
  const top = `${tops.join('\n')}${Object.values(Helpers)
    .map((x) => x.source)
    .join(',')};\n${Variables.size ? `var ${[...Variables].join(',')};` : ''}`
  return { top, program, deps: [...Functions] }
}
