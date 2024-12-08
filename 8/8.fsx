open System.IO

let (--) a b = (fst a - fst b, snd a - snd b)
let (++) a b = (fst a + fst b, snd a + snd b)

let eval collect inputPath = 
  let data = 
    Path.Join(__SOURCE_DIRECTORY__, inputPath)
    |> File.ReadAllLines
    |> Array.map (fun line -> line.ToCharArray())

  let ants =
    [ for r in 0 .. data.Length - 1 do
        for c in 0 .. data[0].Length - 1 do
            (c, r), data[r][c] ]

  let board = ants |> List.map fst

  let pairs = 
    ants
    |> List.filter (snd >> ((=) '.') >> not)
    |> List.allPairs ants
    |> List.filter (fun ant -> 
      let one = fst ant
      let two = snd ant
      one <> two && (snd one) = (snd two))
    |> List.map (fun (a, b) -> fst a, fst b)

  let antinodes = 
    pairs
    |> List.collect (collect board)
    |> Set.ofList

  antinodes.Count

let part1Collector board (a, b) = 
  let diff = a -- b
  [
    if List.contains (a ++ diff) board then 
      a ++ diff
    if List.contains (b -- diff) board then 
      b -- diff
  ]

let part2Collector board (a, b) =
  let diff = a -- b
  let mutable a' = a
  let mutable b' = b
  [
    yield a
    yield b
    while List.contains (a' ++ diff) board do 
      yield a' ++ diff
      a' <- a' ++ diff
    while List.contains (b' -- diff) board do 
      yield b' -- diff
      b' <- b' -- diff
  ]

// part 1
eval part1Collector "example.txt"
eval part1Collector "input.txt" // answer: 276

// part 2
eval part2Collector "example.txt"
eval part2Collector "input.txt" // answer: 991