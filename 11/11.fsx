open System
open System.Collections.Generic

let memoizeR f = 
  let mutable values = Dictionary<_, _>()
  let rec g x =  
    match values.TryGetValue x with
    | true, value -> value
    | _ ->
      let result = f g x
      values.Add (x, result)
      result
  g

let blink recurse (stone: int64, blinksToGo: int64) =
  // if we have zero blinks to go
  // then this stone is no longer splitting, return 1
  if blinksToGo = 0 then 1L
  elif stone = 0 then 
      recurse (1L, blinksToGo - 1L)
  elif stone.ToString().Length % 2 = 0 then
    let items = 
      stone.ToString() 
      |> Seq.splitInto 2 
      |> Seq.map String
      |> Seq.map Convert.ToInt64
      |> Seq.toList
    recurse (items[0], blinksToGo - 1L) + 
      recurse (items[1], blinksToGo - 1L)
  else
    recurse (stone * 2024L, blinksToGo - 1L)

let stones = 
  "4189 413 82070 61 655813 7478611 0 8".Split(' ')
  |> Array.toList
  |> List.map int64

let mBlink = memoizeR blink

// part 1
stones
|> List.fold (fun acc stone -> 
  acc + mBlink (stone, 25)) 0L

// part 2
stones
|> List.fold (fun acc stone -> 
  acc + mBlink (stone, 75)) 0L