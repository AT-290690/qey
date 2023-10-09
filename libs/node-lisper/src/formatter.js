import { APPLY, ATOM, PLACEHOLDER, TOKENS, TYPE, VALUE, WORD } from './enums.js'
const tops = []
const indent = (level) => ' '.repeat(level)
const singleArg = (Arguments) => (Arguments.length === 0 ? '' : ' ')
const traverse = (tree, level = 0) => {
  if (!tree) return ''
  const [first, ...Arguments] = Array.isArray(tree) ? tree : [tree]
  if (first == undefined) return '()'
  const token = first[VALUE]
  if (first[TYPE] === APPLY) {
    switch (token) {
      case TOKENS.BLOCK: {
        if (Arguments.length > 1) {
          return `(${token} ${Arguments.map((x) =>
            traverse(x, ++level).toString()
          ).join(' ')})`
        } else {
          return traverse(Arguments[0], ++level).toString()
        }
      }
      case TOKENS.IMPORT:
        tops.push(
          `(${token}${singleArg(Arguments)}${Arguments.map((x) =>
            traverse(x).toString()
          ).join(' ')})`
        )
        return ''
      case TOKENS.DEFINE_TYPE:
        return `\n(${token}${singleArg(Arguments)}${Arguments.map((x) =>
          traverse(x).toString()
        ).join(' ')})`
      case TOKENS.NUMBER_TYPE:
      case TOKENS.INTEGER_TYPE:
      case TOKENS.BOOLEAN_TYPE:
      case TOKENS.STRING_TYPE:
      case TOKENS.OR_TYPE:
      case TOKENS.AND_TYPE:
      case TOKENS.LAMBDA_TYPE:
      case TOKENS.ATOM:
      case TOKENS.SET_VARIABLE:
      case TOKENS.SET_BOOLEAN:
      case TOKENS.FROM_CHAR_CODE:
      case TOKENS.CHAR_CODE_AT:
      case TOKENS.MAKE_STRING:
      case TOKENS.REGEX_MATCH:
      case TOKENS.REGEX_REPLACE:
      case TOKENS.IS_STRING:
      case TOKENS.IS_NUMBER:
      case TOKENS.IS_INTEGER:
      case TOKENS.IS_FUNCTION:
      case TOKENS.IS_ARRAY:
      case TOKENS.SHORT_ARRAY:
      case TOKENS.ARRAY_TYPE:
      case TOKENS.FUNCTION_TYPE:
        return `(${token}${singleArg(Arguments)}${Arguments.map((x) =>
          traverse(x).toString()
        ).join(' ')})`
      case TOKENS.ARRAY_OR_STRING_LENGTH:
      case TOKENS.FIRST_ARRAY:
      case TOKENS.REST_ARRAY:
      case TOKENS.GET_ARRAY:
      case TOKENS.SET_ARRAY:
      case TOKENS.CALL_FUNCTION:
      case TOKENS.DEFINE_CONSTANT:
      case TOKENS.DEFINE_VARIABLE:
      case TOKENS.DESTRUCTURING_ASSIGMENT:
        return `\n${indent(level)}(${token}${singleArg(
          Arguments
        )}${Arguments.map((x) => traverse(x, ++level)).join(' ')})`
      case TOKENS.ANONYMOUS_FUNCTION:
      case TOKENS.VARIADIC_FUNCTION:
      case TOKENS.TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION:
      case TOKENS.DEFINE_FUNCTION:
      case TOKENS.AND:
      case TOKENS.OR:
      case TOKENS.STRING_CONCATENATION:
      case TOKENS.EQUAL:
      case TOKENS.GREATHER_THAN_OR_EQUAL:
      case TOKENS.LESS_THAN_OR_EQUAL:
      case TOKENS.GREATHER_THAN:
      case TOKENS.LESS_THAN:
      case TOKENS.SUBTRACTION:
      case TOKENS.MULTIPLICATION:
      case TOKENS.DIVISION:
      case TOKENS.ADDITION:
      case TOKENS.BITWISE_AND:
      case TOKENS.BITWISE_OR:
      case TOKENS.BITWISE_XOR:
      case TOKENS.BITWISE_LEFT_SHIFT:
      case TOKENS.BITWISE_RIGHT_SHIFT:
      case TOKENS.BITWISE_UNSIGNED_RIGHT_SHIFT:
      case TOKENS.REMAINDER_OF_DIVISION:
      case TOKENS.BIT_TYPE:
      case TOKENS.BITWISE_NOT:
      case TOKENS.NOT:
      case TOKENS.IF:
      case TOKENS.WHEN:
      case TOKENS.UNLESS:
      case TOKENS.OTHERWISE:
      case TOKENS.CONDITION:
      case TOKENS.CAST_TYPE:
      case TOKENS.PIPE:
      case TOKENS.SLEEP:
      case TOKENS.THROW_ERROR:
      case TOKENS.IMMUTABLE_FUNCTION:
      case TOKENS.IDENTITY:
      case TOKENS.DEBUG:
      case TOKENS.ABORT:
      case TOKENS.NOT_COMPILED_BLOCK:

      default: {
        return `\n${indent(level)}(${token}${singleArg(
          Arguments
        )}${Arguments.map((x) => traverse(x, ++level)).join(' ')})`
      }
    }
  } else if (first[TYPE] === ATOM) {
    return typeof first[VALUE] === 'string'
      ? `"${first[VALUE]}"`
      : first[VALUE].toString()
  } else if (first[TYPE] === WORD) return token
}

export const format = (AST) => {
  tops.length = 0
  const formatted = AST.map((tree) => traverse(tree, 0)).join('\n')
  return `${tops.join('\n').replace(/\n\s*\n\s*\n/g, '')}\n${formatted.replace(
    /\n\s*\n\s*\n/g,
    ''
  )}`
}
