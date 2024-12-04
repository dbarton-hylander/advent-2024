open System
open System.Collections.Generic
open System.IO
open System.Text

let inputPath = Path.Join(__SOURCE_DIRECTORY__, "input.txt")

let inputStr = File.ReadAllText inputPath

let readInt (str: string) = 
  let mutable i = 0
  while Char.IsDigit(str[i]) && i < str.Length - 1 do 
    i <- i + 1
  let finalStr = str[..i - 1]
  if finalStr.Length > 0 && finalStr.Length <= 3 then 
    let intStr = finalStr.ToString()
    Ok (int intStr, str[i..])
  else
    Error()

let processStr (inputStr: string) = 
  let mutable acc = 0
  let mutable idx = 0
  while idx < inputStr.Length - 1 do
    if inputStr[idx .. idx + 3] = "mul(" then
      match readInt inputStr[idx + 4..] with 
      | Ok (first, nextStr) -> 
        if nextStr[0] = ',' then 
          match readInt nextStr[1..] with 
          | Ok (second, nextStr) -> 
            if nextStr[0] = ')' then 
              acc <- acc + first * second
          | Error()-> ()
      | Error() -> ()
      idx <- idx + 4
    else 
      idx <- idx + 1
  acc

processStr inputStr
// part 1 result: 184122457

// part 2
let processStr2 (inputStr: string) = 
  let mutable acc = 0
  let mutable idx = 0
  while idx < inputStr.Length - 1 do
    if inputStr[idx .. idx + 6] = "don't()" then 
      let mutable j = idx + 6
      while j < inputStr.Length - 1 && inputStr[j .. j + 3] <> "do()" do 
        j <- j + 1
      idx <- j + 4
    if inputStr[idx .. idx + 3] = "mul(" then
      match readInt inputStr[idx + 4..] with 
      | Ok (first, nextStr) -> 
        if nextStr[0] = ',' then 
          match readInt nextStr[1..] with 
          | Ok (second, nextStr) -> 
            if nextStr[0] = ')' then 
              acc <- acc + first * second
          | Error()-> ()
      | Error() -> ()
      idx <- idx + 4
    else 
      idx <- idx + 1
  acc

"mul(mul(3,2),2)"

type ExpGrammer = 
  | Mult of int * int
  | Integer of int
  | Trash of string
  | Do of string
  | Dont of string

processStr2 inputStr




