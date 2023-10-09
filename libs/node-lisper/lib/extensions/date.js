import { evaluate } from '../../src/interpreter.js'
export default {
  Extensions: {
    ':date-to-number': (date) => `new Date(${date}).getTime();`,
    ':number-to-date': (number) => `new Date(${number}).toString();`,
  },
  Tops: [],
  env: {
    [':date-to-number']: (args, env) => {
      if (args.length !== 1)
        throw new RangeError(
          'Invalid number of arguments for (:date-to-number)'
        )
      const date = evaluate(args[0], env)
      if (typeof date !== 'string')
        throw new TypeError(
          'First argument of (:date-to-number) is not a string path'
        )
      return new Date(date).getTime()
    },
    [':number-to-date']: (args, env) => {
      if (args.length !== 1)
        throw new RangeError(
          'Invalid number of arguments for (:number-to-date)'
        )
      const number = evaluate(args[0], env)
      if (typeof number !== 'number')
        throw new TypeError(
          'First argument of (:number-to-date) is not a string path'
        )
      return new Date(number).toString()
    },
  },
}
