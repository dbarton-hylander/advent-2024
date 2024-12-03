open System
open System.Collections.Generic
open System.IO

let processFile (processLine: string -> unit) (path: string) = 
  use reader = new StreamReader(path)
  let mutable line = reader.ReadLine()
  while line <> null do
    let trimmed = line.Trim()
    if not <| String.IsNullOrEmpty trimmed then 
      processLine trimmed
    line <- reader.ReadLine()

let tokenize (str: string) = 
  str.Split(" ") |> Array.filter (not << String.IsNullOrWhiteSpace)

let processToken tokenFn = 
  processFile (tokenize >> Array.iteri tokenFn)

let listA = List()
let listB = List()

Path.Join(__SOURCE_DIRECTORY__, "input.txt")
|> processToken (fun idx token -> 
  if idx = 1  then listA.Add (int token)
  else listB.Add (int token))

let sortedA = 
  listA 
  |> List.ofSeq 
  |> List.sort

let sortedB = 
  listB
  |> List.ofSeq
  |> List.sort

sortedA
|> List.zip sortedB
|> List.map (fun (a, b) -> Math.Abs (a - b))
|> List.sum

// part one answer = 1222801

let hashedB = 
  sortedB
  |> List.countBy id

sortedA 
|> List.map (fun key  -> 
  hashedB 
  |> List.tryFind (fun (_key, _value) -> _key = key)
  |> Option.map snd
  |> Option.defaultValue 0
  |> (*) key)
|> List.sum

// part two answer = 22545250
