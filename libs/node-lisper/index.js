import { compileToJs } from './src/compiler.js'
import { evaluate, run } from './src/interpreter.js'
import { parse } from './src/parser.js'
// import wabt from 'wabt'
import {
  isBalancedParenthesis,
  removeNoCode,
  runFromCompiled,
  runFromInterpreted,
  treeShake,
} from './src/utils.js'
import STD from './lib/baked/std.js'
import MATH from './lib/baked/math.js'
import DS from './lib/baked/ds.js'
import { tokens } from './src/tokeniser.js'
import { format } from './src/formatter.js'
const libraries = {
  std: STD,
  math: MATH,
  ds: DS,
}
export default {
  interpred: runFromInterpreted,
  compile: runFromCompiled,
  js: compileToJs,
  format,
  parse,
  tokens,
  evaluate,
  balance: isBalancedParenthesis,
  source: removeNoCode,
  run,
  libraries,
  treeShake,
}
