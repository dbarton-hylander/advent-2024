open System
open System.IO
open System.Text.RegularExpressions

let getXandY (str: string) = 
  let match' = Regex.Match(str, @"X\+(\d+), Y\+(\d+)")
  match'.Groups[1].Value, match'.Groups[2].Value

let getXandYtotal (str: string) = 
  let match' = Regex.Match(str, @"X\=(\d+), Y\=(\d+)")
  match'.Groups[1].Value, match'.Groups[2].Value

let tokenCost (a, b) = 3L * a + b

let solveEquation ((a1, b1, c1), (a2, b2, c2)) =
  let det = a1 * b2 - b1 * a2
  if det = 0L then None
  else
    let a' = c1 * b2 - b1 * c2
    let b' = a1 * c2 - c1 * a2
    if a' % det = 0L && b' % det = 0L then
      let a = a' / det
      let b = b' / det
      Some(a, b)
    else
      None

type GetEq = (string * string) -> (string * string) -> (string * string) -> ((int64 * int64 * int64) * (int64 * int64 * int64))

// fsi forces us to type the parameters. wouldn't have to do this in a .fs file.
let getEqPart1: GetEq = fun (x, y) (x', y') (x'', y'') ->
  (int64 x, int64 x', int64 x''), (int64 y, int64 y', int64 y'')

let getEqPart2: GetEq = fun (x, y) (x', y') (x'', y'') ->
  let t = 10000000000000L
  (int64 x, int64 x', int64 x'' + t), (int64 y, int64 y', int64 y'' + t)

let eval getEq inputPath = 
  Path.Join(__SOURCE_DIRECTORY__, inputPath)
  |> File.ReadAllLines
  |> Array.filter (String.IsNullOrWhiteSpace >> not)
  |> Array.chunkBySize 3
  |> Array.map (fun chunk -> 
    let a = getXandY chunk[0]
    let b = getXandY chunk[1]
    let c = getXandYtotal chunk[2]
    getEq a b c)
  |> Array.map solveEquation
  |> Array.choose id
  |> Array.map tokenCost
  |> Array.sum

// part 1
eval getEqPart1 "example.txt" // 480
eval getEqPart1 "input.txt" // 30413

// part 2
eval getEqPart2 "input.txt" // 92827349540204