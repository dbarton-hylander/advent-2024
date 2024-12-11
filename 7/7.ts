// deno run --allow-read 7.ts
import { join } from "@std/path"

const cartesianProduct = <T>(size: number, elements: T[]): T[][] =>
  Array.from({ length: size }, (_, i) => i + 1).reduce(
    (acc, _) => acc.flatMap(seq => elements.map(e => [...seq, e])),
    [[]] as T[][]
  )

const evaluateExpressions = (
  inputFile: string,
  ops: ((a: number, b: number) => number)[]
): number =>
  Deno.readTextFileSync(join(Deno.cwd(), inputFile))
    .split("\n")
    .map(line => line.trim())
    .filter(line => line.length > 0)
    .map(line => {
      const parts = line.replace(":", "").split(" ").map(Number)
      const [result, ...numbers] = parts
      return { result, numbers }
    })
    .filter(({ result, numbers }) =>
      cartesianProduct(numbers.length - 1, ops).some(operators => {
        const computed = operators.reduce(
          (acc, op, i) => op(acc, numbers[i + 1]),
          numbers[0]
        )
        return computed === result
      })
    )
    .map(({ result }) => result)
    .reduce(add, 0)

const multiply = (a: number, b: number) => a * b
const add = (a: number, b: number) => a + b
const concatFn = (a: number, b: number) => parseInt(`${a}${b}`, 10)

// example
console.log(evaluateExpressions("example.txt", [multiply, add])) // expected: 3749

// part 1
console.log(evaluateExpressions("input.txt", [multiply, add])) // expected: 932137732557

// part 2
console.log(evaluateExpressions("example.txt", [multiply, add, concatFn])) // expected: 11387
console.time("part 2")
console.log(evaluateExpressions("input.txt", [multiply, add, concatFn])) // expected: 661823605105500
console.timeEnd("part 2")
