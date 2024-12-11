#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open FSharp.Collections.ParallelSeq

// let rec cartesianProduct size elements =
//   if size = 0 then [ [] ]
//   else
//     cartesianProduct (size - 1) elements
//     |> List.collect (fun seq -> 
//       elements |> List.map (fun e -> seq @ [e]))

let cartesianProduct size elements =
  [ 1 .. size ]
  |> List.fold (fun acc _ ->
    acc 
    |> List.collect (fun seq -> 
      elements |> List.map (fun e -> seq @ [e]))
     ) [[]]

// cartesianProduct 2 [ 1; 2 ] // [[1; 1]; [1; 2]; [2; 1]; [2; 2]]
// first pass: [[]] -> [[1]; [2]]
// second pass: [[1]; [2]] -> [[1; 1]; [1; 2]; [2; 1]; [2; 2]]


let eval (inputFile: string) (ops: (int64 -> int64 -> int64) list)  = 
  Path.Join(__SOURCE_DIRECTORY__, inputFile)
  |> File.ReadAllLines
  |> Array.map (fun line -> 
    line.Replace(":", "").Split(' ') 
    |> List.ofArray
    |> List.map int64)
  |> Array.map (fun line -> 
      match line with 
      | result :: numbers -> result, numbers
      | _ -> 0, [])
  |> PSeq.filter (fun (result, numbers) ->
      cartesianProduct (numbers.Length - 1) ops
      |> PSeq.exists (fun operators -> 
        let result' = 
          numbers
          |> List.fold (fun acc number -> 
            let idx = fst acc
            let acc = snd acc
            if idx = 0 then (idx + 1, number)
            else (idx + 1, operators[idx - 1] acc number)) (0, 0)
          |> snd
        result' = result))
      // |> PSeq.exists (fun operators -> 
      //   let result' = 
      //     numbers
      //     |> List.tail // Exclude the first number for pairing
      //     |> List.zip operators // Pair each operator with the subsequent number
      //     |> List.fold (fun acc (op, num) -> 
      //       op acc num) (List.head numbers) // Fold starting with the first number
      //   result' = result))
  |> Array.ofSeq
  |> Array.map fst
  |> Array.sum

eval "example.txt" [ (*); (+) ] // 3749

#time
// part 1
eval "input.txt" [ (*); (+) ] // 4998764814652
#time

let concat a b = int64 $"{a}{b}"

eval "example.txt" [ (*); (+); concat ] // 11387L

#time
// part 2
eval "input.txt" [ (*); (+); concat ] // 37598910447546L
#time


let sum accumulator next = accumulator + next

[ 1; 2 ] |> List.fold sum 0

// js equivalent:
// [ 1, 2 ].reduce((acc, next) => acc + next, 0)

let toppings = ["pepperoni"; "mushrooms"; "onions"]

let sizes = ["small"; "medium"; "large"]

let cartesianPizza = 
  sizes 
  |> List.map (fun size -> 
    toppings 
    |> List.map (fun topping -> [ size; topping ]))

let cartesianPizza2 = 
  sizes 
  |> List.collect (fun size -> // switch to collect to flatten
    toppings 
    |> List.map (fun topping -> [ size; topping ]))

let combineTwo lists1 lists2 =
  lists1 
  |> List.collect (fun x -> 
    lists2 
    |> List.map (fun y -> [ x; y ]))

combineTwo sizes toppings

let product1 elementsList =
  [ 1 .. 1 ]
  |> List.fold (fun acc _idx ->
    acc 
    |> List.collect (fun partial -> 
      elementsList |> List.map (fun element -> partial @ [element]))
     ) [[]]

// product1 [ [ 1; 2 ]; [ 3; 4 ]; [5; 6] ]
product1 [ 1; 2 ; 3 ]

// Result: [["small"; "pepperoni"]; ["small"; "mushrooms"]; ...]
