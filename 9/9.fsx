open System.Text
open System.IO

let memory = "233313312141413140234"

String.replicate 4 "11"

let compressed = 
  memory 
  |> Seq.map (string >> int)
  |> Seq.chunkBySize 2
  |> Seq.toList
  |> List.mapi (fun idx pair -> 
    let head = String.replicate pair[0] (string idx)
    let tail = 
      if pair.Length > 1 then 
        String.replicate pair[1] "."
      else ""
    $"{head}{tail}")
  |> List.fold (+) ""

let freeSpaceLen = compressed |> Seq.filter ((=) '.') |> Seq.length
let compressedLen = compressed.Length - freeSpaceLen

let stack1 = compressed.ToCharArray() |> System.Collections.Generic.Stack
let stack2 = compressed.ToCharArray() |> Array.rev |> System.Collections.Generic.Stack

let builder = StringBuilder()

while builder.Length < compressedLen do 
  let next = stack2.Pop() // 0 
  if next = '.' then 
    let mutable s2 = stack1.Pop()
    while s2 = '.' do 
      s2 <- stack1.Pop()
    builder.Append s2 |> ignore
  else 
    builder.Append next |> ignore
  
let str = builder.ToString()

str 
|> Seq.map (string >> int)
|> Seq.fold (fun acc next -> 
  let idx = fst acc
  let pos = idx * next
  printfn $"{idx}: {idx} * {next} = {pos}"
  // printfn $"sum: {next + pos}"
  (idx + 1, snd acc + pos)) (0, 0)
|> snd
