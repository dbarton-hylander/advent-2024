open System
open System.IO

let quadrant (w, h) = 
  let midX = Math.Ceiling(float w / 2.0) |> int
  let midY = Math.Ceiling(float h / 2.0) |> int
  fun (x, y) ->
    let x', y' = x + 1, y + 1
    if x' = midX || y' = midY then None
    else
      match x' <= midX, y' <= midY with
      | true, true -> Some 0
      | false, true -> Some 1
      | true, false -> Some 2
      | false, false -> Some 3

let (+%) a b = 
  let x = a % b
  if x < 0 then x + b else x

let (%%) a b = fst a +% fst b, snd a +% snd b

let parseTuple (str: string) = 
  let parts =
    str.Split("=").[1].Split(",") 
    |> Array.map int
  (parts[0], parts[1])

let eval space time inputFile = 
  let quad = quadrant space
  Path.Join(__SOURCE_DIRECTORY__, inputFile)
  |> File.ReadAllLines
  |> Array.map(fun line -> 
    let parts = line.Split(" ")
    let p = parseTuple parts[0]
    let v = parseTuple parts[1]
    (p, v))
  |> Array.map (fun ((px, py), (vx, vy)) -> 
    (px + vx * time, py + vy * time) %% space)
  |> Array.map quad
  |> Array.choose id
  |> Array.countBy id
  |> Array.map snd
  |> Array.fold (*) 1

// part 1
eval (11, 7) 100 "example.txt" // 12
eval (101, 103) 100 "input.txt" // 208437768

// part 2
let printTree inputFile space = 
  let printSpace (w, h) finalPositions =
    for y in 0 .. h - 1 do
      for x in 0 .. w - 1 do
        if finalPositions 
          |> Array.exists (fun (fx, fy) -> 
            fx = x && fy = y) then printf "X"
        else printf "."
      printfn ""
  let data =  
    Path.Join(__SOURCE_DIRECTORY__, inputFile)
      |> File.ReadAllLines
      |> Array.map(fun line -> 
        let parts = line.Split(" ")
        let p = parseTuple parts[0]
        let v = parseTuple parts[1]
        (p, v))
  fun time ->
    data
    |> Array.map (fun ((px, py), (vx, vy)) -> 
      (px + vx * time, py + vy * time) %% space)
    |> printSpace space

let printTree' = printTree "input.txt" (101, 103)
  
// part 2 should be run and found in a console
printfn "Press Enter to move forward, Backspace to move backward"

let mutable time = 6000
let mutable paused = false
while true do
  match Console.ReadKey(true).Key with
  | ConsoleKey.Enter -> 
    Console.Clear()
    printfn "Time: %d" time
    printTree' time
    time <- time + 1
  | ConsoleKey.Backspace -> 
    time <- time - 1
    paused <- true
    Console.Clear()
    printfn "Time: %d" time
    printTree' time
  | _ -> ()
  System.Threading.Thread.Sleep(1000/60)