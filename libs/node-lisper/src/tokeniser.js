import {
  APPLY,
  ATOM,
  TYPE,
  VALUE,
  WORD,
  TYPES,
  PLACEHOLDER,
  TOKENS,
} from './enums.js'
import { evaluate } from './interpreter.js'
const stringifyType = (type) =>
  Array.isArray(type)
    ? `(array ${type.map((t) => stringifyType(t)).join(' ')})`
    : typeof type
const stringifyArgs = (args) =>
  args
    .map((x) => {
      return Array.isArray(x)
        ? `(${stringifyArgs(x)})`
        : x[TYPE] === APPLY || x[TYPE] === WORD
        ? x[VALUE]
        : JSON.stringify(x[VALUE])
            .replace(new RegExp(/\[/g), '(')
            .replace(new RegExp(/\]/g), ')')
            .replace(new RegExp(/\,/g), ' ')
            .replace(new RegExp(/"/g), '')
    })
    .join(' ')
const isForbiddenVariableName = (name) => {
  switch (name) {
    case '_':
    case TOKENS.CAST_TYPE:
    case TOKENS.DEFINE_CONSTANT:
    case TOKENS.DEFINE_VARIABLE:
    case TOKENS.DEFINE_TYPE:
    case TOKENS.DEFINE_FUNCTION:
    case TOKENS.DESTRUCTURING_ASSIGMENT:
      return true
    default:
      return false
  }
}
const atom = (arg, env) => {
  if (arg[TYPE] === ATOM) return 1
  else {
    const atom = evaluate(arg, env)
    return +(
      typeof atom === 'number' ||
      typeof atom === 'bigint' ||
      typeof atom === 'string'
    )
  }
}
const equal = (a, b) =>
  (typeof a !== 'object' && typeof b !== 'object' && typeof a === typeof b) ||
  (Array.isArray(a) &&
    Array.isArray(b) &&
    (!a.length ||
      !b.length ||
      !(a.length > b.length ? a : b).some(
        (_, i, bigger) =>
          !equal(
            bigger.at(i),
            (a.length > b.length ? b : a).at(
              i % (a.length > b.length ? b : a).length
            )
          )
      ))) ||
  false
const partial = (a, b) =>
  (typeof a !== 'object' && typeof b !== 'object' && typeof a === typeof b) ||
  (Array.isArray(a) &&
    Array.isArray(b) &&
    (!a.length ||
      !b.length ||
      !(a.length < b.length ? a : b).some(
        (_, i, smaller) =>
          !equal(
            smaller.at(i),
            (a.length < b.length ? b : a).at(
              i % (a.length < b.length ? b : a).length
            )
          )
      ))) ||
  false
const tokens = {
  [TOKENS.LAMBDA_TYPE]: (args, env) => args.map((x) => evaluate(x, env)),
  [TOKENS.OR_TYPE]: (args, env) => {
    const out = args.map((x) => evaluate(x, env))
    out._type = TOKENS.OR_TYPE
    return out
  },
  [TOKENS.AND_TYPE]: (args, env) => {
    const out = args.map((x) => evaluate(x, env))
    out._type = TOKENS.AND_TYPE
    return out
  },
  [TOKENS.DEFINE_TYPE]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.DEFINE_TYPE}) (2 required) (${
          TOKENS.DEFINE_TYPE
        } ${stringifyArgs(args)})`
      )
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.DEFINE_TYPE}) (2 required) (${
          TOKENS.DEFINE_TYPE
        } ${stringifyArgs(args)})`
      )

    const word = args[0]
    if (word[TYPE] !== WORD)
      throw new SyntaxError(
        `First argument of (${TOKENS.DEFINE_TYPE}) must be word but got ${
          word[TYPE]
        } (${TOKENS.DEFINE_TYPE} ${stringifyArgs(args)})`
      )
    const name = word[VALUE]
    const T = args[1]
    if (!env[TYPES]) env[TYPES] = {}
    env[TYPES][name] = evaluate(T, env)
    return env[TYPES][name]
  },
  [TOKENS.IDENTITY]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.IDENTITY
        }), expected 1 but got ${args.length}. (${
          TOKENS.IDENTITY
        } ${stringifyArgs(args)})`
      )
    return evaluate(args[0], env)
  },
  [TOKENS.STRING_CONCATENATION]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          tokens.STRING_CONCATENATION
        }), expected > 1 but got ${args.length}. (${
          tokens.STRING_CONCATENATION
        } ${stringifyArgs(args)}).`
      )
    const operands = args.map((x) => evaluate(x, env))
    if (operands.some((x) => typeof x !== 'string'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.STRING_CONCATENATION}) are (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.STRING_CONCATENATION} ${stringifyArgs(args)}).`
      )
    return operands.reduce((a, b) => a + b, '')
  },
  [TOKENS.REMAINDER_OF_DIVISION]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.REMAINDER_OF_DIVISION
        }), expected > 1 but got ${args.length}. (${
          TOKENS.REMAINDER_OF_DIVISION
        } ${stringifyArgs(args)}).`
      )
    const [a, b] = args.map((x) => evaluate(x, env))
    if (typeof a !== 'number' || typeof b !== 'number')
      throw new TypeError(
        `Not all arguments of (${TOKENS.REMAINDER_OF_DIVISION}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.REMAINDER_OF_DIVISION} ${stringifyArgs(args)}).`
      )
    if (b === 0)
      throw new TypeError(
        `Second argument of (${
          TOKENS.REMAINDER_OF_DIVISION
        }) can't be a (0) (division by 0 is not allowed) (${
          TOKENS.REMAINDER_OF_DIVISION
        } ${stringifyArgs(args)}).`
      )

    return a % b
  },
  [TOKENS.DIVISION]: (args, env) => {
    if (!args.length) return 0
    if (args.length === 1) {
      const number = evaluate(args[0], env)
      if (typeof number !== 'number')
        throw new TypeError(
          `Arguments of (${TOKENS.DIVISION}) is not a (${
            TOKENS.NUMBER_TYPE
          }) (${TOKENS.DIVISION} ${stringifyArgs(args)}).`
        )
      if (number === 0)
        throw new TypeError(
          `Argument of (${
            TOKENS.DIVISION
          }) can't be a (0) (division by 0 is not allowed) (${
            TOKENS.DIVISION
          } ${stringifyArgs(args)}).`
        )
      return 1 / number
    }
    const operands = args.map((x) => evaluate(x, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.DIVISION}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.DIVISION} ${stringifyArgs(args)}).`
      )
    return operands.reduce((a, b) => a / b)
  },
  [TOKENS.ARRAY_OR_STRING_LENGTH]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.ARRAY_OR_STRING_LENGTH
        }) (1 required) (${TOKENS.ARRAY_OR_STRING_LENGTH} ${stringifyArgs(
          args
        )}).`
      )
    const array = evaluate(args[0], env)
    if (!(Array.isArray(array) || typeof array === 'string'))
      throw new TypeError(
        `First argument of (${TOKENS.ARRAY_OR_STRING_LENGTH}) must be an (or ${
          TOKENS.ARRAY_TYPE
        } ${TOKENS.STRING_TYPE}) (${
          TOKENS.ARRAY_OR_STRING_LENGTH
        } ${stringifyArgs(args)}).`
      )
    return array.length
  },
  [TOKENS.IS_ARRAY]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.IS_ARRAY}) (1 required) (${
          TOKENS.IS_ARRAY
        } ${stringifyArgs(args)}).`
      )
    const array = evaluate(args[0], env)
    return +Array.isArray(array)
  },
  [TOKENS.IS_NUMBER]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.IS_NUMBER}) (1 required) (${
          TOKENS.IS_NUMBER
        } ${stringifyArgs(args)}).`
      )
    return +(typeof evaluate(args[0], env) === 'number')
  },
  [TOKENS.IS_INTEGER]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.IS_INTEGER}) (1 required) (${
          TOKENS.IS_INTEGER
        } ${stringifyArgs(args)}).`
      )
    return +(typeof evaluate(args[0], env) === 'bigint')
  },
  [TOKENS.IS_STRING]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.IS_STRING}) (1 required) (${
          TOKENS.IS_STRING
        } ${stringifyArgs(args)}).`
      )
    return +(typeof evaluate(args[0], env) === 'string')
  },
  [TOKENS.IS_FUNCTION]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.IS_FUNCTION
        }) (1 required) (${TOKENS.IS_FUNCTION} ${stringifyArgs(args)}).`
      )
    return +(typeof evaluate(args[0], env) === 'function')
  },
  [TOKENS.CHAR_CODE_AT]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.CHAR_CODE_AT
        }) (2 required) (${TOKENS.CHAR_CODE_AT} ${stringifyArgs(args)}).`
      )
    const string = evaluate(args[0], env)
    if (typeof string !== 'string')
      throw new TypeError(
        `First argument of (${TOKENS.CHAR_CODE_AT}) must be an (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.CHAR_CODE_AT} ${stringifyArgs(args)}).`
      )
    const index = evaluate(args[1], env)
    if (!Number.isInteger(index) || index < 0)
      throw new TypeError(
        `Second argument of (${TOKENS.CHAR_CODE_AT}) must be an (+ ${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.CHAR_CODE_AT} ${stringifyArgs(args)}).`
      )
    return string.charCodeAt(index)
  },
  [TOKENS.FROM_CHAR_CODE]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.FROM_CHAR_CODE
        }) (= 1 required) (${TOKENS.FROM_CHAR_CODE} ${stringifyArgs(args)}).`
      )
    const index = evaluate(args[0], env)
    if (!Number.isInteger(index) || index < 0)
      throw new TypeError(
        `Arguments of (${TOKENS.FROM_CHAR_CODE}) must be (+ ${
          TOKENS.INTEGER_TYPE
        }) (${TOKENS.FROM_CHAR_CODE} ${stringifyArgs(args)}).`
      )
    return String.fromCharCode(index)
  },
  [TOKENS.MAKE_STRING]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.MAKE_STRING
        }) (= 1 required) (${TOKENS.MAKE_STRING} ${stringifyArgs(args)}).`
      )
    const indexes = evaluate(args[0], env)
    if (indexes.some((index) => !Number.isInteger(index) || index < 0))
      throw new TypeError(
        `Arguments of (${TOKENS.MAKE_STRING}) must be (+ ${
          TOKENS.INTEGER_TYPE
        }) (${TOKENS.MAKE_STRING} ${stringifyArgs(args)}).`
      )
    return String.fromCharCode(...indexes)
  },
  [TOKENS.ADDITION]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.ADDITION
        }), expected > 1 but got ${args.length}. (${
          TOKENS.ADDITION
        } ${stringifyArgs(args)}).`
      )
    const operands = args.map((x) => evaluate(x, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.ADDITION}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.ADDITION} ${stringifyArgs(args)}).`
      )
    return operands.reduce((a, b) => a + b)
  },
  [TOKENS.MULTIPLICATION]: (args, env) => {
    if (!args.length) return 1
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.MULTIPLICATION}), expected (or (> 1) (= 0)) but got ${args.length}.`
      )
    const operands = args.map((x) => evaluate(x, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.MULTIPLICATION}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.MULTIPLICATION} ${stringifyArgs(args)}).`
      )
    return operands.reduce((a, b) => a * b)
  },
  [TOKENS.SUBTRACTION]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.SUBTRACTION
        }), expected >= 1 but got ${args.length}. (${
          TOKENS.SUBTRACTION
        } ${stringifyArgs(args)}).`
      )
    const operands = args.map((x) => evaluate(x, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.SUBTRACTION}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.SUBTRACTION} ${stringifyArgs(args)}).`
      )
    return args.length === 1 ? -operands[0] : operands.reduce((a, b) => a - b)
  },
  [TOKENS.IF]: (args, env) => {
    if (args.length < 2 || args.length > 3)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.IF
        }), expected (or 2 3) but got ${args.length} (${
          TOKENS.IF
        } ${stringifyArgs(args)}).`
      )
    return evaluate(args[0], env)
      ? evaluate(args[1], env)
      : evaluate(args[2], env)
  },
  [TOKENS.UNLESS]: (args, env) => {
    if (args.length < 2 || args.length > 3)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.UNLESS
        }), expected (or 2 3)  but got ${args.length} (${
          TOKENS.UNLESS
        } ${stringifyArgs(args)}).`
      )
    return evaluate(args[0], env)
      ? evaluate(args[2], env)
      : evaluate(args[1], env)
  },
  [TOKENS.WHEN]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.WHEN}), expected 2 but got ${
          args.length
        } (${TOKENS.WHEN} ${stringifyArgs(args)}).`
      )
    if (evaluate(args[0], env)) return evaluate(args[1], env)
    return 0
  },
  [TOKENS.OTHERWISE]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.OTHERWISE
        }), expected 2 but got ${args.length} (${
          TOKENS.OTHERWISE
        } ${stringifyArgs(args)}).`
      )
    if (evaluate(args[0], env)) return 0
    return evaluate(args[1], env)
  },
  [TOKENS.CONDITION]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.CONDITION
        }), expected (> 2 required) but got ${args.length} (${
          TOKENS.CONDITION
        } ${stringifyArgs(args)}).`
      )
    for (let i = 0; i < args.length; i += 2) {
      if (evaluate(args[i], env)) return evaluate(args[i + 1], env)
    }
    return 0
  },
  [TOKENS.ARRAY_TYPE]: (args, env) => {
    if (!args.length) return []
    const isCapacity =
      args.length === 2 && args[1][TYPE] === WORD && args[1][VALUE] === 'length'
    if (isCapacity) {
      if (args.length !== 2)
        throw new RangeError(
          `Invalid number of arguments for (${
            TOKENS.ARRAY_TYPE
          }) (= 2 required) (${TOKENS.ARRAY_TYPE} ${stringifyArgs(args)})`
        )
      const N = evaluate(args[0], env)
      if (!Number.isInteger(N))
        throw new TypeError(
          `Size argument for (${TOKENS.ARRAY_TYPE}) has to be an (32 bit ${
            TOKENS.INTEGER_TYPE
          }) (${TOKENS.ARRAY_TYPE} ${stringifyArgs(args)})`
        )
      return new Array(N).fill(0)
    }
    return args.map((x) => evaluate(x, env))
  },
  [TOKENS.SHORT_ARRAY]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.SHORT_ARRAY
        }) (>= 1 required) (${TOKENS.SHORT_ARRAY} ${stringifyArgs(args)}).`
      )
    return args.map((x) => evaluate(x, env))
  },
  [TOKENS.ATOM]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.ATOM}) (1 required) (${
          TOKENS.ATOM
        } ${stringifyArgs(args)}).`
      )
    return atom(args[0], env)
  },
  [TOKENS.FIRST_ARRAY]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.FIRST_ARRAY
        }) (1 required) (${TOKENS.FIRST_ARRAY} ${stringifyArgs(args)}).`
      )
    const array = evaluate(args[0], env)
    if (!Array.isArray(array))
      throw new TypeError(
        `Argument of (${TOKENS.FIRST_ARRAY}) must be an (${
          TOKENS.ARRAY_TYPE
        }) (${TOKENS.FIRST_ARRAY} ${stringifyArgs(args)}).`
      )
    if (array.length === 0)
      throw new RangeError(
        `Argument of (${TOKENS.FIRST_ARRAY}) is an empty (${
          TOKENS.ARRAY_TYPE
        }) (${TOKENS.FIRST_ARRAY} ${stringifyArgs(args)}).`
      )
    const value = array.at(0)
    if (value == undefined)
      throw new RangeError(
        `Trying to get a null value in (${TOKENS.ARRAY_TYPE}) at (${
          TOKENS.FIRST_ARRAY
        }) (${TOKENS.FIRST_ARRAY} ${stringifyArgs(args)}).`
      )
    return value
  },
  [TOKENS.REST_ARRAY]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.REST_ARRAY}) (1 required) (${
          TOKENS.REST_ARRAY
        } ${stringifyArgs(args)})`
      )
    const array = evaluate(args[0], env)
    if (!Array.isArray(array))
      throw new TypeError(
        `Argument of (${TOKENS.REST_ARRAY}) must be an (${
          TOKENS.ARRAY_TYPE
        }) (${TOKENS.REST_ARRAY} ${stringifyArgs(args)}).`
      )
    if (array.length === 0)
      throw new RangeError(
        `Argument of (${TOKENS.REST_ARRAY}) is an empty (${
          TOKENS.ARRAY_TYPE
        }) (${TOKENS.REST_ARRAY} ${stringifyArgs(args)}).`
      )
    return array.slice(1)
  },
  [TOKENS.GET_ARRAY]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.GET_ARRAY}) (2 required) (${
          TOKENS.GET_ARRAY
        } ${stringifyArgs(args)})`
      )
    const array = evaluate(args[0], env)
    if (!Array.isArray(array))
      throw new TypeError(
        `First argument of (${TOKENS.GET_ARRAY}) must be an (${
          TOKENS.ARRAY_TYPE
        })) (${TOKENS.GET_ARRAY} ${stringifyArgs(args)}).`
      )
    if (array.length === 0)
      throw new RangeError(
        `First argument of (${TOKENS.GET_ARRAY}) is an empty (${
          TOKENS.ARRAY_TYPE
        })) (${TOKENS.GET_ARRAY} ${stringifyArgs(args)})).`
      )
    const index = evaluate(args[1], env)
    if (!Number.isInteger(index))
      throw new TypeError(
        `Second argument of (${TOKENS.GET_ARRAY}) must be an (32 bit ${
          TOKENS.INTEGER_TYPE
        }) (${index}) (${TOKENS.GET_ARRAY} ${stringifyArgs(args)}).`
      )
    if (index > array.length - 1 || index * -1 > array.length)
      throw new RangeError(
        `Second argument of (${TOKENS.GET_ARRAY}) is outside of the (${
          TOKENS.ARRAY_TYPE
        }) bounds (${index}) (${TOKENS.GET_ARRAY} ${stringifyArgs(args)}).`
      )
    const value = array.at(index)
    if (value == undefined)
      throw new RangeError(
        `Trying to get a null value in (${TOKENS.ARRAY_TYPE}) at (${
          TOKENS.GET_ARRAY
        }) (${TOKENS.GET_ARRAY} ${stringifyArgs(args)}).`
      )
    return value
  },
  [TOKENS.SET_ARRAY]: (args, env) => {
    if (args.length !== 2 && args.length !== 3)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.SET_ARRAY
        }) (or 2 3) required (${TOKENS.SET_ARRAY} ${stringifyArgs(args)})`
      )
    const array = evaluate(args[0], env)
    if (!Array.isArray(array))
      throw new TypeError(
        `First argument of (${TOKENS.SET_ARRAY}) must be an (${
          TOKENS.ARRAY_TYPE
        }) but got (${array}) (${TOKENS.SET_ARRAY} ${stringifyArgs(args)}).`
      )
    const index = evaluate(args[1], env)
    if (!Number.isInteger(index))
      throw new TypeError(
        `Second argument of (${TOKENS.SET_ARRAY}) must be an (32 bit ${
          TOKENS.INTEGER_TYPE
        }) (${index}) (${TOKENS.SET_ARRAY} ${stringifyArgs(args)}).`
      )
    if (index > array.length)
      throw new RangeError(
        `Second argument of (${TOKENS.SET_ARRAY}) is outside of the (${
          TOKENS.ARRAY_TYPE
        }) bounds (index ${index} bounds ${array.length}) (${
          TOKENS.SET_ARRAY
        } ${stringifyArgs(args)}).`
      )
    if (index < 0) {
      if (args.length !== 2)
        throw new RangeError(
          `Invalid number of arguments for (${
            TOKENS.SET_ARRAY
          }) (if (< index 0) then 2 required) (${
            TOKENS.SET_ARRAY
          } ${stringifyArgs(args)})`
        )
      if (index * -1 > array.length)
        throw new RangeError(
          `Second argument of (${TOKENS.SET_ARRAY}) is outside of the (${
            TOKENS.ARRAY_TYPE
          }) bounds (index ${index} bounds ${array.length}) (${
            TOKENS.SET_ARRAY
          } ${stringifyArgs(args)})`
        )
      const target = array.length + index
      while (array.length !== target) array.pop()
    } else {
      if (args.length !== 3)
        throw new RangeError(
          `Invalid number of arguments for (${
            TOKENS.SET_ARRAY
          }) (if (>= index 0) then 3 required) (${
            TOKENS.SET_ARRAY
          } ${stringifyArgs(args)})`
        )
      const value = evaluate(args[2], env)
      if (value == undefined)
        throw new RangeError(
          `Trying to set a null value in (${TOKENS.ARRAY_TYPE}) at (${
            TOKENS.SET_ARRAY
          }). (${TOKENS.SET_ARRAY} ${stringifyArgs(args)})`
        )
      array[index] = value
    }
    return array
  },
  [TOKENS.LOG]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.LOG}) (>= 1 required) (${
          TOKENS.LOG
        } ${stringifyArgs(args)})`
      )
    const expressions = args.map((x) => evaluate(x, env))
    console.log(...expressions)
    return expressions.at(-1)
  },
  [TOKENS.BLOCK]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.BLOCK}) (>= 1 required) (${
          TOKENS.BLOCK
        } ${stringifyArgs(args)})`
      )
    return args.reduce((_, x) => evaluate(x, env), 0)
  },
  [TOKENS.DEFINE_FUNCTION]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.DEFINE_FUNCTION
        }) (2 required) (${TOKENS.DEFINE_FUNCTION} ${stringifyArgs(args)})`
      )
    const params = args.slice(1, -1)
    const body = args.at(-1)
    const name = args[0][VALUE]
    if (isForbiddenVariableName(name))
      throw new ReferenceError(
        `Function name ${name} is forbidden at (${
          TOKENS.DEFINE_FUNCTION
        } ${stringifyArgs(args)})`
      )
    if (params.some((param) => isForbiddenVariableName(param[VALUE])))
      throw new ReferenceError(
        `Argument name ${JSON.stringify(
          params.find((param) => isForbiddenVariableName(param[VALUE]))
        )} is forbidden at (${TOKENS.DEFINE_FUNCTION} ${stringifyArgs(args)})`
      )
    const fn = (props = [], scope) => {
      if (props.length > params.length) {
        throw new RangeError(
          `More arguments for (${TOKENS.DEFINE_FUNCTION} ${name} ${params
            .map((x) => x[VALUE])
            .join(' ')
            .trim()}) are provided. (expects ${params.length} but got ${
            props.length
          })`
        )
      }
      if (props.length !== params.length)
        throw new RangeError(
          `Incorrect number of arguments for (${
            TOKENS.DEFINE_FUNCTION
          } ${name} ${params
            .map((x) => x[VALUE])
            .join(' ')
            .trim()}) are provided. (expects ${params.length} but got ${
            props.length
          }) (${TOKENS.DEFINE_FUNCTION} ${stringifyArgs(args)})`
        )
      const localEnv = Object.create(env)
      const isTyped = env[TYPES] && name in env[TYPES]
      if (isTyped && props.length !== env[TYPES][name].length - 1)
        throw new RangeError(
          `The number of arguments doesn't match type of ${name}\n(${
            TOKENS.DEFINE_FUNCTION
          } ${stringifyArgs(args)})\n should be ${env[TYPES][name].length - 1}`
        )
      for (let i = 0; i < props.length; ++i) {
        const value = evaluate(props[i], scope)
        const param = params[i][VALUE]
        if (isTyped) {
          const Type = env[TYPES][name].slice(0, -1)
          if (
            Type[i]._type === TOKENS.OR_TYPE &&
            !Type[i].some((T) => equal(value, T))
          )
            throw new TypeError(
              `Type doesn't match ${i} argument (${param}) of ${name}\n(${
                TOKENS.DEFINE_FUNCTION
              } ${stringifyArgs(args)})\nShould be:\n${Type[i]
                .map((T) => stringifyType(T))
                .join('\nor\n')}`
            )
          else if (
            Type[i]._type === TOKENS.AND_TYPE &&
            Type[i].length &&
            !Type[i].some((T) => partial(value, T))
          ) {
            throw new TypeError(
              `Type doesn't match ${i} argument (${param}) of ${name}\n(${
                TOKENS.DEFINE_FUNCTION
              } ${stringifyArgs(args)})\nShould be:\n${Type[i]
                .map((T) => stringifyType(T))
                .join('\nand\n')}`
            )
          }
        }
        Object.defineProperty(localEnv, param, {
          value,
          writable: true,
        })
      }
      const result = evaluate(body, localEnv)
      if (isTyped) {
        const Type = env[TYPES][name].at(-1)
        if (
          Type._type === TOKENS.OR_TYPE &&
          !Type.some((T) => equal(result, T))
        )
          throw new TypeError(
            `Type doesn't match result of ${name}\n(${
              TOKENS.DEFINE_FUNCTION
            } ${stringifyArgs(args)})\nShould be:\n${Type.map((T) =>
              stringifyType(T)
            ).join('\nor\n')}`
          )
        else if (
          Type._type === TOKENS.AND_TYPE &&
          Type.length &&
          !Type.some((T) => partial(result, T))
        ) {
          throw new TypeError(
            `Type doesn't match result of ${name}\n(${
              TOKENS.DEFINE_FUNCTION
            } ${stringifyArgs(args)})\nShould be:\n${Type.map((T) =>
              stringifyType(T)
            ).join('\nand\n')}`
          )
        }
      }
      return result
    }
    env[name] = fn
    return fn
  },
  [TOKENS.ANONYMOUS_FUNCTION]: (args, env) => {
    const params = args.slice(0, -1)
    const body = args.at(-1)
    return (props = [], scope) => {
      if (props.length !== params.length)
        throw new RangeError(
          `Incorrect number of arguments for (${
            TOKENS.ANONYMOUS_FUNCTION
          } ${params.map((x) => x[VALUE]).join(' ')}) are provided. (expects ${
            params.length
          } but got ${props.length}) (${
            TOKENS.ANONYMOUS_FUNCTION
          } ${stringifyArgs(args)})`
        )
      const localEnv = Object.create(env)
      for (let i = 0; i < props.length; ++i) {
        Object.defineProperty(localEnv, params[i][VALUE], {
          value: evaluate(props[i], scope),
          writable: true,
        })
      }
      return evaluate(body, localEnv)
    }
  },
  [TOKENS.VARIADIC_FUNCTION]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.VARIADIC_FUNCTION
        }) (2 required) (${TOKENS.VARIADIC_FUNCTION} ${stringifyArgs(args)})`
      )
    const [params, body] = args
    return (props = [], scope) => {
      const localEnv = Object.create(env)
      Object.defineProperty(localEnv, params[VALUE], {
        value: props.map((arg) => evaluate(arg, scope)),
        writable: true,
      })
      return evaluate(body, localEnv)
    }
  },
  [TOKENS.NOT]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.NOT}) (1 required) (${
          TOKENS.NOT
        } ${stringifyArgs(args)})`
      )
    return +!evaluate(args[0], env)
  },
  [TOKENS.EQUAL]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.EQUAL}) (2 required) (${
          TOKENS.EQUAL
        } ${stringifyArgs(args)})`
      )
    const a = evaluate(args[0], env)
    const b = evaluate(args[1], env)
    if (
      Array.isArray(a) ||
      Array.isArray(b) ||
      typeof a === 'function' ||
      typeof b === 'function'
    )
      throw new TypeError(
        `Invalid use of (${TOKENS.EQUAL}), some arguments are not an ${
          TOKENS.ATOM
        } (${TOKENS.EQUAL} ${stringifyArgs(args)})`
      )
    return +(a === b)
  },
  [TOKENS.LESS_THAN]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.LESS_THAN}) (2 required) (${
          TOKENS.LESS_THAN
        } ${stringifyArgs(args)})`
      )
    const a = evaluate(args[0], env)
    const b = evaluate(args[1], env)
    if (
      Array.isArray(a) ||
      Array.isArray(b) ||
      typeof a === 'function' ||
      typeof b === 'function'
    )
      throw new TypeError(
        `Invalid use of (${TOKENS.LESS_THAN}), some arguments are not an ${
          TOKENS.ATOM
        } (${TOKENS.LESS_THAN} ${stringifyArgs(args)})`
      )
    return +(a < b)
  },
  [TOKENS.GREATHER_THAN]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.GREATHER_THAN
        }) (2 required) (${TOKENS.GREATHER_THAN} ${stringifyArgs(args)})`
      )
    const a = evaluate(args[0], env)
    const b = evaluate(args[1], env)
    if (
      Array.isArray(a) ||
      Array.isArray(b) ||
      typeof a === 'function' ||
      typeof b === 'function'
    )
      throw new TypeError(
        `Invalid use of (${TOKENS.GREATHER_THAN}), some arguments are not an ${
          TOKENS.ATOM
        } (${TOKENS.GREATHER_THAN} ${stringifyArgs(args)})`
      )
    return +(a > b)
  },
  [TOKENS.GREATHER_THAN_OR_EQUAL]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.GREATHER_THAN_OR_EQUAL
        }) (2 required) (${TOKENS.GREATHER_THAN_OR_EQUAL} ${stringifyArgs(
          args
        )})`
      )
    const a = evaluate(args[0], env)
    const b = evaluate(args[1], env)
    if (
      Array.isArray(a) ||
      Array.isArray(b) ||
      typeof a === 'function' ||
      typeof b === 'function'
    )
      throw new TypeError(
        `Invalid use of (${
          TOKENS.GREATHER_THAN_OR_EQUAL
        }), some arguments are not an ${TOKENS.ATOM} (${
          TOKENS.GREATHER_THAN_OR_EQUAL
        } ${stringifyArgs(args)})`
      )
    return +(a >= b)
  },
  [TOKENS.LESS_THAN_OR_EQUAL]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${
          TOKENS.LESS_THAN_OR_EQUAL
        }) (2 required) (${TOKENS.LESS_THAN_OR_EQUAL} ${stringifyArgs(args)})`
      )
    const a = evaluate(args[0], env)
    const b = evaluate(args[1], env)
    if (
      Array.isArray(a) ||
      Array.isArray(b) ||
      typeof a === 'function' ||
      typeof b === 'function'
    )
      throw new TypeError(
        `Invalid use of (${
          TOKENS.LESS_THAN_OR_EQUAL
        }), some arguments are not an ${TOKENS.ATOM} (${
          TOKENS.LESS_THAN_OR_EQUAL
        } ${stringifyArgs(args)})`
      )
    return +(a <= b)
  },
  [TOKENS.AND]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.AND}) (>= 2 required) (${
          TOKENS.AND
        } ${stringifyArgs(args)})`
      )
    let circuit
    for (let i = 0; i < args.length - 1; ++i) {
      circuit = evaluate(args[i], env)
      if (circuit) continue
      else return circuit
    }
    return evaluate(args.at(-1), env)
  },
  [TOKENS.OR]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.OR}) (>= 2 required) (${
          TOKENS.OR
        } ${stringifyArgs(args)})`
      )
    let circuit
    for (let i = 0; i < args.length - 1; ++i) {
      circuit = evaluate(args[i], env)
      if (circuit) return circuit
      else continue
    }
    return evaluate(args.at(-1), env)
  },
  [TOKENS.CALL_FUNCTION]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.CALL_FUNCTION
        }) (>= 1 required) (${TOKENS.CALL_FUNCTION} ${stringifyArgs(args)})`
      )
    const [first, ...rest] = args
    if (first[TYPE] === WORD && first[VALUE] in tokens)
      throw new TypeError(
        `Following argument of (${
          TOKENS.CALL_FUNCTION
        }) must not be an reserved word (${
          TOKENS.CALL_FUNCTION
        } ${stringifyArgs(args)})`
      )
    const apply = evaluate(first, env)
    if (typeof apply !== 'function')
      throw new TypeError(
        `First argument of (${TOKENS.CALL_FUNCTION}) must be a (${
          TOKENS.ANONYMOUS_FUNCTION
        }) (${TOKENS.CALL_FUNCTION} ${stringifyArgs(args)})`
      )

    return apply(rest, env)
  },
  [TOKENS.DESTRUCTURING_ASSIGMENT]: (args, env) => {
    if (args.length < 3)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.DESTRUCTURING_ASSIGMENT
        }) (>= 3 required) (${TOKENS.DESTRUCTURING_ASSIGMENT} ${stringifyArgs(
          args
        )})`
      )
    const right = evaluate(args.pop(), env)
    if (!Array.isArray(right))
      throw new TypeError(
        `Last argument of (${TOKENS.DESTRUCTURING_ASSIGMENT}) has to be an (${
          TOKENS.ARRAY_TYPE
        }) (${TOKENS.DESTRUCTURING_ASSIGMENT} ${stringifyArgs(args)})`
      )
    const rest = args.pop()
    for (let i = 0; i < args.length; ++i) {
      const word = args[i]
      if (word[TYPE] !== WORD)
        throw new SyntaxError(
          `Variable argument of (${
            TOKENS.DESTRUCTURING_ASSIGMENT
          }) must be word but got ${word[TYPE]} (${
            TOKENS.DESTRUCTURING_ASSIGMENT
          } ${stringifyArgs(args)})`
        )
      if (word[VALUE] !== PLACEHOLDER) {
        if (right[i] == undefined)
          throw new ReferenceError(
            `Attempting to assign a void value at (${
              TOKENS.DESTRUCTURING_ASSIGMENT
            }) when asssigning ${word[VALUE]} (${
              TOKENS.DESTRUCTURING_ASSIGMENT
            } ${stringifyArgs(args)})`
          )
        else if (isForbiddenVariableName(word[VALUE]))
          throw new ReferenceError(
            `Variable name ${word[VALUE]} is forbidden at (${
              TOKENS.DESTRUCTURING_ASSIGMENT
            } ${stringifyArgs(args)})`
          )
        Object.defineProperty(env, word[VALUE], {
          value: right[i],
          writable: false,
        })
      }
    }
    if (rest[TYPE] !== WORD)
      throw new SyntaxError(
        `Rest argument of (${
          TOKENS.DESTRUCTURING_ASSIGMENT
        }) must be word but got ${rest[TYPE]} (${
          TOKENS.DESTRUCTURING_ASSIGMENT
        } ${stringifyArgs(args)})`
      )
    if (rest[VALUE] !== PLACEHOLDER) {
      Object.defineProperty(env, rest[VALUE], {
        value: right.slice(args.length),
        writable: false,
      })
      if (isForbiddenVariableName(rest[VALUE]))
        throw new ReferenceError(
          `Variable name ${rest[VALUE]} is forbidden at (${
            TOKENS.DESTRUCTURING_ASSIGMENT
          } ${stringifyArgs(args)})`
        )
    }
    return right
  },
  [TOKENS.DEFINE_CONSTANT]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.DEFINE_CONSTANT
        }) (> 2 required) (${TOKENS.DEFINE_CONSTANT} ${stringifyArgs(args)})`
      )
    if (args.length % 2 === 1)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.DEFINE_CONSTANT
        }) (pairs of 2 required) (${TOKENS.DEFINE_CONSTANT} ${stringifyArgs(
          args
        )})`
      )
    let name
    for (let i = 0; i < args.length; ++i) {
      if (i % 2 === 0) {
        const word = args[i]
        if (word[TYPE] !== WORD)
          throw new SyntaxError(
            `First argument of (${
              TOKENS.DEFINE_CONSTANT
            }) must be word but got ${word[TYPE]} (${
              TOKENS.DEFINE_CONSTANT
            } ${stringifyArgs(args)})`
          )
        else if (isForbiddenVariableName(word[VALUE]))
          throw new ReferenceError(
            `Variable name ${word[VALUE]} is forbidden at (${
              TOKENS.DEFINE_CONSTANT
            } ${stringifyArgs(args)})`
          )
        name = word[VALUE]
      } else
        Object.defineProperty(env, name, {
          value: evaluate(args[i], env),
          writable: false,
        })
    }
    return env[name]
  },
  [TOKENS.DEFINE_VARIABLE]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.DEFINE_VARIABLE
        }) (> 2 required) (${TOKENS.DEFINE_VARIABLE} ${stringifyArgs(args)})`
      )
    if (args.length % 2 === 1)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.DEFINE_VARIABLE
        }) (pairs of 2 required) (${TOKENS.DEFINE_VARIABLE} ${stringifyArgs(
          args
        )})`
      )
    let name
    for (let i = 0; i < args.length; ++i) {
      if (i % 2 === 0) {
        const word = args[i]
        if (word[TYPE] !== WORD)
          throw new SyntaxError(
            `First argument of (${
              TOKENS.DEFINE_VARIABLE
            }) must be word but got ${word[TYPE]} (${
              TOKENS.DEFINE_VARIABLE
            } ${stringifyArgs(args)})`
          )
        else if (isForbiddenVariableName(word[VALUE]))
          throw new ReferenceError(
            `Variable name ${word[VALUE]} is forbidden at (${
              TOKENS.DEFINE_VARIABLE
            } ${stringifyArgs(args)})`
          )

        name = word[VALUE]
      } else env[name] = evaluate(args[i], env)
    }
    return env[name]
  },
  [TOKENS.SET_VARIABLE]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.SET_VARIABLE
        }) (2 required) (${TOKENS.SET_VARIABLE} ${stringifyArgs(args)})`
      )
    const entityName = args[0][VALUE]
    const value = evaluate(args[1], env)
    for (let scope = env; scope; scope = Object.getPrototypeOf(scope))
      if (Object.prototype.hasOwnProperty.call(scope, entityName)) {
        if (typeof scope[entityName] !== typeof value)
          throw new TypeError(
            `Invalid use of (${
              TOKENS.SET_VARIABLE
            }), variable must be assigned to the same type (${
              Array.isArray(scope[entityName])
                ? `array`
                : `${typeof scope[entityName]}`
            }) but attempted to assign to (${
              Array.isArray(value) ? 'array' : typeof value
            }) (${TOKENS.SET_VARIABLE} ${stringifyArgs(args)})`
          )
        scope[entityName] = value
        return value
      }
    throw new ReferenceError(
      `Tried setting an undefined variable: ${entityName} using (${
        TOKENS.SET_VARIABLE
      }) (${TOKENS.SET_VARIABLE} ${stringifyArgs(args)})`
    )
  },
  [TOKENS.SET_BOOLEAN]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.SET_BOOLEAN}) (2 required) (${
          TOKENS.SET_BOOLEAN
        } ${stringifyArgs(args)})`
      )
    const entityName = args[0][VALUE]
    const value = evaluate(args[1], env)
    if (value !== 0 && value !== 1)
      throw new TypeError(
        `Invalid use of (${
          TOKENS.SET_BOOLEAN
        }), value must be either (or 0 1) (${
          TOKENS.SET_BOOLEAN
        } ${stringifyArgs(args)})`
      )
    for (let scope = env; scope; scope = Object.getPrototypeOf(scope))
      if (Object.prototype.hasOwnProperty.call(scope, entityName)) {
        if (scope[entityName] !== 0 && scope[entityName] !== 1)
          throw new TypeError(
            `Invalid use of (${
              TOKENS.SET_BOOLEAN
            }), variable must be either (or 0 1) (${
              TOKENS.SET_BOOLEAN
            } ${stringifyArgs(args)})`
          )
        scope[entityName] = value
        return value
      }
    throw new ReferenceError(
      `Tried setting an undefined variable: ${entityName} using (${
        TOKENS.SET_BOOLEAN
      }) (${TOKENS.SET_BOOLEAN} ${stringifyArgs(args)})`
    )
  },
  [TOKENS.IMPORT]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.IMPORT}) (>= 2 required) (${
          TOKENS.IMPORT
        } ${stringifyArgs(args)})`
      )
    const [first, ...rest] = args
    const module = evaluate(first, env)
    if (typeof module !== 'function')
      throw new TypeError(
        `First argument of (${TOKENS.IMPORT}) must be an (function) but got (${
          first[VALUE]
        }). (${TOKENS.IMPORT} ${stringifyArgs(args)})`
      )
    const functions = rest.map((arg) => evaluate(arg, env))
    if (functions.some((arg) => typeof arg !== 'string'))
      throw new TypeError(
        `Following arguments of (${TOKENS.IMPORT} ${
          first[VALUE]
        }) must all be (${TOKENS.STRING_TYPE}). (${
          TOKENS.IMPORT
        } ${stringifyArgs(args)})`
      )
    const records = functions.reduce((a, b) => (a.add(b), a), new Set())
    const library = module().filter(
      ([key]) => records.has(key) && records.delete(key)
    )
    if (records.size)
      throw new ReferenceError(
        `Library ${first[VALUE]} doesn't have a functions (${[...records].join(
          ' '
        )}) at (${TOKENS.IMPORT} ${first[VALUE]})`
      )
    library.forEach(([key, fn]) => (env[key] = fn))

    return functions
  },
  [TOKENS.STRING_TYPE]: () => '',
  [TOKENS.NUMBER_TYPE]: () => 0,
  [TOKENS.INTEGER_TYPE]: () => 0n,
  [TOKENS.BOOLEAN_TYPE]: () => 1,
  [TOKENS.FUNCTION_TYPE]: () => () => {},
  [TOKENS.CAST_TYPE]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments for (${TOKENS.CAST_TYPE}) ${args.length}`
      )
    const type = args[1][VALUE]
    const value = evaluate(args[0], env)
    if (value == undefined)
      throw ReferenceError(
        `Trying to access undefined value at (${TOKENS.CAST_TYPE})`
      )
    if (args.length === 2) {
      switch (type) {
        case TOKENS.NUMBER_TYPE: {
          const num = Number(value)
          if (isNaN(num))
            throw new TypeError(
              `Attempting to convert Not a ${
                TOKENS.NUMBER_TYPE
              } ("${value}") to a ${TOKENS.NUMBER_TYPE} at (${
                TOKENS.CAST_TYPE
              }) (${TOKENS.CAST_TYPE} ${stringifyArgs(args)}).`
            )
          return num
        }
        case TOKENS.INTEGER_TYPE:
          return BigInt(value)
        case TOKENS.STRING_TYPE:
          return value.toString()
        case TOKENS.BIT_TYPE:
          return parseInt(value, 2)
        case TOKENS.BOOLEAN_TYPE:
          return +!!value
        case TOKENS.FUNCTION_TYPE:
          return () => value
        case TOKENS.ARRAY_TYPE: {
          if (typeof value === 'number' || typeof value === 'bigint')
            return [...Number(value).toString()].map(Number)
          else if (typeof value[Symbol.iterator] !== 'function')
            throw new TypeError(
              `Arguments are not iterable for ${TOKENS.ARRAY_TYPE} at (${
                TOKENS.CAST_TYPE
              }) (${TOKENS.CAST_TYPE} ${stringifyArgs(args)}).`
            )
          return [...value]
        }
        default:
          throw new TypeError(
            `Can only cast (or ${TOKENS.NUMBER_TYPE} ${TOKENS.INTEGER_TYPE} ${
              TOKENS.STRING_TYPE
            } ${TOKENS.ARRAY_TYPE} ${TOKENS.BIT_TYPE} ${
              TOKENS.BOOLEAN_TYPE
            }) at (${TOKENS.CAST_TYPE}) (${TOKENS.CAST_TYPE} ${stringifyArgs(
              args
            )}).`
          )
      }
    }
  },
  [TOKENS.BIT_TYPE]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.BIT_TYPE}) (1 required). (${
          TOKENS.BIT_TYPE
        } ${stringifyArgs(args)})`
      )
    const operand = evaluate(args[0], env)
    if (typeof operand !== 'number' && typeof operand !== 'bigint')
      throw new TypeError(
        `Argument of (${TOKENS.BIT_TYPE}) is not a (${TOKENS.NUMBER_TYPE}) (${
          TOKENS.BIT_TYPE
        } ${stringifyArgs(args)}).`
      )
    return (operand >>> 0).toString(2)
  },
  [TOKENS.BITWISE_AND]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_AND
        }) (>= 2 required). (${TOKENS.BITWISE_AND} ${stringifyArgs(args)})`
      )
    const operands = args.map((a) => evaluate(a, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.BITWISE_AND}) are (or ${
          TOKENS.NUMBER_TYPE
        } ${TOKENS.INTEGER_TYPE}) (${TOKENS.BITWISE_AND} ${stringifyArgs(
          args
        )}).`
      )
    return operands.reduce((acc, x) => acc & x)
  },
  [TOKENS.BITWISE_NOT]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_NOT
        }) (1 required). (${TOKENS.BITWISE_NOT} ${stringifyArgs(args)})`
      )
    const operand = evaluate(args[0], env)
    if (typeof operand !== 'number' && typeof operand !== 'bigint')
      throw new TypeError(
        `Argument of (${TOKENS.BITWISE_NOT}) is not a (or ${
          TOKENS.NUMBER_TYPE
        } ${TOKENS.INTEGER_TYPE}) (${TOKENS.BITWISE_NOT} ${stringifyArgs(
          args
        )}).`
      )
    return ~operand
  },
  [TOKENS.BITWISE_OR]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_OR
        }) (>= 2 required). (${TOKENS.BITWISE_OR} ${stringifyArgs(args)})`
      )
    const operands = args.map((a) => evaluate(a, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.BITWISE_OR}) are (or ${
          TOKENS.NUMBER_TYPE
        } ${TOKENS.INTEGER_TYPE}) (${TOKENS.BITWISE_OR} ${stringifyArgs(
          args
        )}).`
      )
    return operands.reduce((acc, x) => acc | x)
  },
  [TOKENS.BITWISE_XOR]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_XOR
        }) (>= 2 required). (${TOKENS.BITWISE_XOR} ${stringifyArgs(args)}).`
      )
    const operands = args.map((a) => evaluate(a, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.BITWISE_XOR}) are (or ${
          TOKENS.NUMBER_TYPE
        } ${TOKENS.INTEGER_TYPE}) (${TOKENS.BITWISE_XOR} ${stringifyArgs(
          args
        )}).`
      )
    return operands.reduce((acc, x) => acc ^ x)
  },
  [TOKENS.BITWISE_LEFT_SHIFT]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_LEFT_SHIFT
        }) (>= 2 required). (${TOKENS.BITWISE_LEFT_SHIFT} ${stringifyArgs(
          args
        )}).`
      )
    const operands = args.map((a) => evaluate(a, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.BITWISE_LEFT_SHIFT}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.BITWISE_LEFT_SHIFT} ${stringifyArgs(args)}).`
      )
    return operands.reduce((acc, x) => acc << x)
  },
  [TOKENS.BITWISE_RIGHT_SHIFT]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_RIGHT_SHIFT
        }) (>= 2 required). (${TOKENS.BITWISE_RIGHT_SHIFT} ${stringifyArgs(
          args
        )}).`
      )
    const operands = args.map((a) => evaluate(a, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.BITWISE_RIGHT_SHIFT}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.BITWISE_RIGHT_SHIFT} ${stringifyArgs(args)}).`
      )
    return operands.reduce((acc, x) => acc >> x)
  },
  [TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT]: (args, env) => {
    if (args.length < 2)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT
        }) (>= 2 required). (${
          TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT
        } ${stringifyArgs(args)}).`
      )
    const operands = args.map((a) => evaluate(a, env))
    if (operands.some((x) => typeof x !== 'number' && typeof x !== 'bigint'))
      throw new TypeError(
        `Not all arguments of (${TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT}) are (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT} ${stringifyArgs(args)}).`
      )
    return operands.reduce((acc, x) => acc >>> x)
  },
  [TOKENS.PIPE]: (args, env) => {
    if (args.length < 1)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.PIPE}) (>= 1 required). (${
          TOKENS.PIPE
        } ${stringifyArgs(args)})`
      )
    let inp = args[0]
    for (let i = 1; i < args.length; ++i) {
      if (!Array.isArray(args[i]))
        throw new TypeError(
          `Argument at position (${i}) of (${TOKENS.PIPE}) is not a (${
            TOKENS.FUNCTION_TYPE
          }). (${TOKENS.PIPE} ${stringifyArgs(args)})`
        )
      const [first, ...rest] = args[i]
      const arr = [first, inp, ...rest]
      inp = arr
    }
    return evaluate(inp, env)
  },
  [TOKENS.THROW_ERROR]: (args, env) => {
    if (args.length !== 1)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.THROW_ERROR
        }) (1 required). (${TOKENS.THROW_ERROR} ${stringifyArgs(args)}).`
      )
    const string = evaluate(args[0], env)
    if (typeof string !== 'string')
      throw new TypeError(
        `First argument of (${TOKENS.THROW_ERROR}) must be an (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.THROW_ERROR} ${stringifyArgs(args)}).`
      )
    throw new Error(string)
  },
  [TOKENS.TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION
        }) (>= 2 required). (${
          TOKENS.TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION
        } ${stringifyArgs(args)}).`
      )
    // TODO: Add validation for TCO recursion
    const [definition, ...functionArgs] = args
    const token = definition[VALUE]
    if (!(token in tokens))
      throw new ReferenceError(
        `There is no such keyword ${token} at (${
          TOKENS.TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION
        } ${stringifyArgs(args)})`
      )
    return tokens[token](functionArgs, env)
  },
  [TOKENS.IMMUTABLE_FUNCTION]: (args, env) => {
    if (!args.length)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.IMMUTABLE_FUNCTION
        }) (>= 2 required). (${TOKENS.IMMUTABLE_FUNCTION} ${stringifyArgs(
          args
        )}).`
      )
    const [definition, ...functionArgs] = args
    const token = definition[VALUE]
    if (!(token in tokens))
      throw new ReferenceError(
        `There is no such keyword ${token} at (${
          TOKENS.IMMUTABLE_FUNCTION
        } ${stringifyArgs(args)})`
      )
    const fn = tokens[token](functionArgs, {
      ...tokens,
      [TYPES]: env[TYPES],
    })
    env[functionArgs[0][VALUE]] = fn
    return fn
  },
  [TOKENS.REGEX_MATCH]: (args, env) => {
    if (args.length !== 2)
      throw new RangeError(
        `Invalid number of arguments to (${TOKENS.REGEX_MATCH}) (2 required) (${
          TOKENS.REGEX_MATCH
        } ${stringifyArgs(args)})`
      )
    const string = evaluate(args[0], env)
    if (typeof string !== 'string')
      throw new TypeError(
        `First argument of (${TOKENS.REGEX_MATCH}) has to be a (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.REGEX_MATCH} ${stringifyArgs(args)})`
      )
    const regex = evaluate(args[1], env)
    if (typeof regex !== 'string')
      throw new TypeError(
        `Second argument of (${TOKENS.REGEX_MATCH}) has to be a ((${
          TOKENS.STRING_TYPE
        }) (${TOKENS.REGEX_MATCH} ${stringifyArgs(args)})`
      )
    const match = string.match(new RegExp(regex, 'g'))
    return match == undefined ? [] : [...match]
  },
  [TOKENS.REGEX_REPLACE]: (args, env) => {
    if (args.length !== 3)
      throw new RangeError(
        `Invalid number of arguments to (${
          TOKENS.REGEX_REPLACE
        }) (3 required) (${TOKENS.REGEX_REPLACE} ${stringifyArgs(args)})`
      )
    const string = evaluate(args[0], env)
    if (typeof string !== 'string')
      throw new TypeError(
        `First argument of (${TOKENS.REGEX_REPLACE}) has to be a (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.REGEX_REPLACE} ${stringifyArgs(args)})`
      )
    const a = evaluate(args[1], env)
    if (typeof a !== 'string')
      throw new TypeError(
        `Second argument of (${TOKENS.REGEX_REPLACE}) has to be a (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.REGEX_REPLACE} ${stringifyArgs(args)})`
      )
    const b = evaluate(args[2], env)
    if (typeof b !== 'string')
      throw new TypeError(
        `Third argument of (${TOKENS.REGEX_REPLACE}) has to be a (${
          TOKENS.STRING_TYPE
        }) (${TOKENS.REGEX_REPLACE} ${stringifyArgs(args)})`
      )
    return string.replace(new RegExp(a, 'g'), b)
  },
  [TOKENS.SLEEP]: (args, env) => {
    const time = evaluate(args[0], env)
    if (typeof time !== 'number')
      throw new TypeError(
        `First argument of (${TOKENS.SLEEP}) is not a (${
          TOKENS.NUMBER_TYPE
        }) (${TOKENS.SLEEP} ${stringifyArgs(args)}).`
      )
    const callback = evaluate(args[1], env)
    if (typeof callback !== 'function')
      throw new TypeError(
        `Second argument of (${TOKENS.SLEEP}) is not a (${
          TOKENS.FUNCTION_TYPE
        }) (${TOKENS.SLEEP} ${stringifyArgs(args)}).`
      )
    return setTimeout(callback, time)
  },
  [TOKENS.DEBUG]: (_, env) => {
    let out = {}
    let total = 0
    for (const item in env) {
      const current = env[item]
      if (current._count) {
        total += current._count
        out[item] = current._count
      }
    }
    return [Object.entries(out), ['⏱️ ', total]]
  },
  [TOKENS.ABORT]: () => process.exit(),
}
tokens[TOKENS.NOT_COMPILED_BLOCK] = tokens[TOKENS.BLOCK]
export { tokens }
