open System
open System.IO

type Scope<'a> = 
  {
    empty: 'a
    sum: 'a -> 'a -> 'a
    collect: int * int -> 'a
    sumBy: 'a -> int
  }

let eval scope inputPath =
  let data = 
    Path.Join(__SOURCE_DIRECTORY__, inputPath)
    |> File.ReadAllLines
    |> Array.map (fun l -> 
      l.ToCharArray()
      |> Array.map (Char.GetNumericValue >> int))

  let rows = data.Length
  let cols = data[0].Length

  let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]

  let inBounds r c = r >= 0 && c >= 0 && r < rows && c < cols

  let rec explore (r: int, c: int) =
    let current = data[r][c]
    if current = 9 then scope.collect (r, c)
    else
      directions
      |> List.choose (fun (dr, dc) ->
          let nr, nc = r + dr, c + dc
          if inBounds nr nc && data[nr][nc] = current + 1 then
            Some (explore (nr, nc))
          else
            None)
      |> List.fold scope.sum scope.empty

  let trailheads =
    [ for r in 0 .. rows - 1 do
      for c in 0 .. cols - 1 do
        if data[r][c] = 0 then yield (r, c) ]

  trailheads
  |> List.sumBy (explore >> scope.sumBy)

// part 1
eval {
  empty = Set.empty
  sum = Set.union
  collect = Set.singleton
  sumBy = _.Count
} "example.txt"
eval {
  empty = Set.empty
  sum = Set.union
  collect = Set.singleton
  sumBy = _.Count
} "input.txt" // 820
// part 2
eval {
  empty = 0
  sum = (+)
  collect = fun _ -> 1
  sumBy = id
} "input.txt" // 1786