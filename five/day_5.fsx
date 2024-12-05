open System
open System.IO

let inputPath = Path.Join(__SOURCE_DIRECTORY__, "input.txt")
let examplePath = Path.Join(__SOURCE_DIRECTORY__, "example.txt")

let eq (a, b) = a = b
let thunk<'a, 't> (a: 't) = fun (_discard: 'a) -> a

let processInput (filePath: string) (filterFn: string array * string array -> bool) = 
  let str = File.ReadAllText filePath
  let lines = str.Split("\n")

  let sortingRules = 
    lines
    |> Array.map _.Split("|")
    |> Array.filter (fun arr -> arr.Length = 2)
    |> Array.map (fun arr -> arr[0], arr[1])
    |> Array.groupBy fst

  let updates = 
    lines 
    |> Array.map _.Split(",")
    |> Array.filter (fun arr -> arr.Length > 1)
  
  let sorter a b = 
    sortingRules 
    |> Array.tryFind (fst >> (=) a)
    |> Option.map snd
    |> Option.bind (Array.map snd >> Array.tryFind ((=) b))

  updates
  |> Array.map (fun update ->
    let sorted = 
      update
      |> Array.sortWith (fun a b -> 
        sorter a b
        |> Option.map (thunk -1)
        |> Option.orElseWith(fun () -> 
          sorter b a |> Option.map (thunk 1))
        |> Option.defaultValue 0)
    update, sorted)
  |> Array.filter filterFn
  |> Array.map snd
  |> Array.map (fun update -> 
    // return the middle idx
    update[int (Math.Floor(float update.Length / 2.))])
  |> Array.map int
  |> Array.sum

// part one
processInput examplePath eq // 143
processInput inputPath eq  // 4569
// part two
processInput examplePath (not << eq) // 123
processInput inputPath (not << eq) // 6456