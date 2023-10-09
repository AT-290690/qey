import { APPLY, ATOM, TYPE, VALUE, WORD } from './enums.js'
import {
  handleUnbalancedParens,
  handleUnbalancedQuotes,
  removeNoCode,
} from './utils.js'
export const parse = (source) => {
  source = handleUnbalancedQuotes(handleUnbalancedParens(removeNoCode(source)))
  const tree = []
  let head = tree,
    stack = [tree],
    acc = ''
  for (let i = 0; i < source.length; ++i) {
    const cursor = source[i]
    if (cursor === '"') {
      acc += '"'
      ++i
      while (source[i] !== '"') {
        if (source[i] === '\\')
          switch (source[++i]) {
            case '\\':
              acc += '\\'
              break
            case 'n':
              acc += '\n'
              break
            case 'r':
              acc += '\r'
              break
            case 't':
              acc += '\t'
              break
            case 's':
              acc += '\\s'
              break
            case '"':
              acc += '"'
              break
          }
        else acc += source[i]
        ++i
      }
    }
    if (cursor === '(') {
      head.push([])
      stack.push(head)
      head = head.at(-1)
    } else if (cursor === ')' || cursor === ' ') {
      let token = acc
      acc = ''
      if (token) {
        if (!head.length) head.push({ [TYPE]: APPLY, [VALUE]: token })
        else if (token.match(/^"([^"]*)"/))
          head.push({
            [TYPE]: ATOM,
            [VALUE]: token.substring(1, token.length - 1),
          })
        else if (token.match(/^-?[0-9]\d*(\.\d+)?$/))
          head.push({
            [TYPE]: ATOM,
            [VALUE]: Number(token),
          })
        else head.push({ [TYPE]: WORD, [VALUE]: token })
      }
      if (cursor === ')') head = stack.pop()
    } else acc += cursor
  }
  return tree
}
