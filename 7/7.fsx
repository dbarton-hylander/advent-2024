#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open FSharp.Collections.ParallelSeq

let cartesianProduct size elements =
  [ 1 .. size ]
  |> List.fold (fun acc _ ->
    acc 
    |> List.collect (fun seq -> 
      elements |> List.map (fun e -> seq @ [e]))
     ) [[]]

let eval (inputFile: string) (ops: (int64 -> int64 -> int64) list)  = 
  // read input
  Path.Join(__SOURCE_DIRECTORY__, inputFile)
  |> File.ReadAllLines
  // parse input 
  |> Array.map (fun strLine -> 
    let line = 
      strLine.Replace(":", "").Split(' ') 
      |> List.ofArray
      |> List.map int64
    match line with 
    | result :: numbers -> result, numbers
    | _ -> 0, [])
  // filter out the results that don't match the expected result
  |> PSeq.filter (fun (result, numbers) ->
      cartesianProduct (numbers.Length - 1) ops
      |> PSeq.exists (fun operators -> 
        let result' = 
          numbers
          |> List.tail // Exclude the first number for pairing
          |> List.zip operators // Pair each operator with the subsequent number
          |> List.fold (fun acc (op, num) -> 
            op acc num) (List.head numbers) // Fold starting with the first number
        result' = result))
  |> Seq.map fst
  |> Seq.sum

eval "example.txt" [ (*); (+) ] // 3749

#time
// part 1
eval "input.txt" [ (*); (+) ] // 932137732557
#time

let concat a b = int64 $"{a}{b}"
eval "example.txt" [ (*); (+); concat ] // 11387L

#time
// part 2
eval "input.txt" [ (*); (+); concat ] // 661823605105500
#time
