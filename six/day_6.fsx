open System.IO

let data = 
  Path.Join(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (fun line -> line.Trim().ToCharArray())

let height = data.Length
let width = data[0].Length

// find the starting position of the guard
// it's the cell that isn't '#' or '.'
let mutable startpos = (0, 0)
for iy in 0..height - 1 do
  for ix in 0..width - 1 do
    if data[iy][ix] <> '#' && data[iy][ix] <> '.' then
      startpos <- (iy, ix)

// why: these directions map current direction to the next direction after a right turn
let rotateDirection = 
  Map.ofList [
    (0, 1), (1, 0)
    (-1, 0), (0, 1)
    (1, 0), (0, -1)
    (0, -1), (-1, 0)
  ]

let walk (fakeObstacle: int * int) =
  let mutable pos = startpos
  let mutable dir = -1, 0
  let mutable guardPath = Set.empty
  let mutable doneWalking = false

  while not doneWalking do
    let nextPosition = (fst pos + fst dir, snd pos + snd dir)
    
    let offBoard = 
      fst nextPosition < 0 || 
      fst nextPosition >= height || 
      snd nextPosition < 0 || 
      snd nextPosition >= width
    
    let loopDetected = guardPath.Contains (nextPosition, dir)
    
    // if we're out of bounds, we're done
    if offBoard then
      doneWalking <- true
    elif loopDetected then
      guardPath <- Set.empty
      doneWalking <- true
    else
      let isObstacle = 
        if nextPosition = fakeObstacle then true
        else data[fst nextPosition][snd nextPosition] = '#'
      if isObstacle then
        dir <- rotateDirection[dir]
      else
        guardPath <- guardPath.Add (nextPosition, dir)
        pos <- nextPosition
  guardPath

let guardPath = 
  walk (-1, -1)
  |> Seq.map fst
  |> Set.ofSeq

let part1 = guardPath.Count + 1 // +1 for the starting position

// part 1 answer = 4454

// for p2, sum how many positions in p1been cause loop when blocked
#time
let part2 =
  guardPath
  |> Seq.map walk
  |> Seq.filter Set.isEmpty
  |> Seq.length
#time
// part 2 answer = 1503


