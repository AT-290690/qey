// AST enums
export const WORD = 'w'
export const APPLY = 'f'
export const VALUE = 'v'
export const TYPE = 't'
export const ATOM = 'a'
// tokeniser enums
export const TYPES = ';'
export const PLACEHOLDER = '.'
// tokens aliases
export const TOKENS = {
  BIT_TYPE: 'Bit',
  LAMBDA_TYPE: 'Lambda',
  STRING_TYPE: 'String',
  NUMBER_TYPE: 'Number',
  INTEGER_TYPE: 'Integer',
  BOOLEAN_TYPE: 'Boolean',
  FUNCTION_TYPE: 'Function',
  ARRAY_TYPE: 'Array',

  CAST_TYPE: 'type',

  OR_TYPE: 'Or',
  AND_TYPE: 'And',

  DEFINE_TYPE: 'deftype',

  IDENTITY: 'identity',
  STRING_CONCATENATION: 'concatenate',
  ARRAY_OR_STRING_LENGTH: 'length',
  IS_ARRAY: 'Array?',
  IS_NUMBER: 'Number?',
  IS_INTEGER: 'Integer?',
  IS_STRING: 'String?',
  IS_FUNCTION: 'Function?',
  CHAR_CODE_AT: 'char-code',
  FROM_CHAR_CODE: 'char',
  MAKE_STRING: 'make-string',

  ADDITION: '+',
  SUBTRACTION: '-',
  MULTIPLICATION: '*',
  DIVISION: '/',
  REMAINDER_OF_DIVISION: 'mod',

  BITWISE_AND: '&',
  BITWISE_OR: '|',
  BITWISE_NOT: '~',
  BITWISE_XOR: '^',
  BITWISE_LEFT_SHIFT: '<<',
  BITWISE_RIGHT_SHIFT: '>>',
  BITWISE_UNSIGNED_RIGHT_SHIFT: '>>>',
  ATOM: 'atom?',

  SHORT_ARRAY: "'",
  FIRST_ARRAY: 'car',
  REST_ARRAY: 'cdr',
  GET_ARRAY: 'get',
  SET_ARRAY: 'set',

  LOG: 'log',
  BLOCK: 'do',

  DEFINE_FUNCTION: 'defun',
  ANONYMOUS_FUNCTION: 'lambda',
  VARIADIC_FUNCTION: 'function',

  IF: 'if',
  UNLESS: 'unless',
  WHEN: 'when',
  OTHERWISE: 'otherwise',
  CONDITION: 'cond',

  NOT: 'not',
  EQUAL: '=',
  LESS_THAN: '<',
  GREATHER_THAN: '>',
  GREATHER_THAN_OR_EQUAL: '>=',
  LESS_THAN_OR_EQUAL: '<=',
  AND: 'and',
  OR: 'or',
  CALL_FUNCTION: 'apply',
  DESTRUCTURING_ASSIGMENT: 'destructuring-bind',
  DEFINE_CONSTANT: 'defconstant',
  DEFINE_VARIABLE: 'defvar',
  SET_VARIABLE: 'setf',
  SET_BOOLEAN: 'boole',
  IMPORT: 'import',

  PIPE: 'go',
  THROW_ERROR: 'throw',
  TAIL_CALLS_OPTIMISED_RECURSIVE_FUNCTION: 'loop',
  IMMUTABLE_FUNCTION: 'safety',
  NOT_COMPILED_BLOCK: 'void',
  REGEX_MATCH: 'regex-match',
  REGEX_REPLACE: 'regex-replace',
  SLEEP: 'sleep',

  DEBUG: 'debug',
  ABORT: 'abort',
}
