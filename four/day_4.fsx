open System
open System.Collections
open System.Collections.Generic
open System.IO

type Direction = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft

type Matrix(inputPath: string) = 
  let inputStr = File.ReadAllText inputPath

  let lines = 
    inputStr.Split("\n")
    |> Array.map _.Trim()
    |> Array.filter (not << String.IsNullOrEmpty)
    |> Array.map _.ToCharArray()

  member _.Get(x: int, y: int) = 
    if x < 0 || x >= lines[0].Length || y < 0 || y >= lines.Length then 
      '.'
    else
      lines[y][x]

  interface IEnumerable<Point> with 
    member this.GetEnumerator (): IEnumerator<Point> = 
      seq {
        for y in 0 .. lines.Length - 1 do 
          for x in 0 .. lines[y].Length - 1 do
            yield Point(x, y, this)
      }
      |> _.GetEnumerator()

  interface IEnumerable with 
    member this.GetEnumerator (): IEnumerator = 
      (this :> IEnumerable<Point>).GetEnumerator()

and Point(x: int, y: int, matrix: Matrix) = 
  member this.Value = matrix.Get(x, y)

  member this.Scan(direction: Direction, count: int): string = 
    let mutable x = x
    let mutable y = y
    let tempStr = List<char>()
    for i in 0 .. count - 1 do
      tempStr.Add(matrix.Get(x, y))   
      match direction with 
      | Up -> y <- y - 1
      | UpRight -> x <- x + 1; y <- y - 1 
      | Right -> x <- x + 1
      | DownRight -> x <- x + 1; y <- y + 1
      | Down -> y <- y + 1
      | DownLeft -> y <- y + 1; x <- x - 1
      | Left -> x <- x - 1
      | UpLeft -> x <- x - 1; y <- y - 1
    String(tempStr.ToArray()) 

  member this.Move(direction: Direction) = 
    match direction with 
    | Up -> Point(x, y - 1, matrix)
    | UpRight -> Point(x + 1, y - 1, matrix) 
    | Right -> Point(x + 1, y, matrix)
    | DownRight -> Point(x + 1, y + 1, matrix)
    | Down -> Point(x, y + 1, matrix)
    | DownLeft -> Point(x - 1, y + 1, matrix)
    | Left -> Point(x - 1, y, matrix)
    | UpLeft -> Point(x - 1, y - 1, matrix)

let partOne (matrix: Matrix) =
  matrix
  |> Seq.map (fun point -> 
    [
      point.Scan(Up, 4) // true
      point.Scan(UpRight, 4)
      point.Scan(Right, 4)
      point.Scan(DownRight, 4)
      point.Scan(Down, 4)
      point.Scan(DownLeft, 4)
      point.Scan(Left, 4)
      point.Scan(UpLeft, 4)
    ] 
    |> Seq.filter (fun scan -> scan = "XMAS")
    |> Seq.length
  )
  |> Seq.sum

let partTwo (matrix: Matrix) = 
  let xmas = Set(['M'; 'A'; 'S'])
  matrix
  |> Seq.filter (fun point -> point.Value = 'A')
  |> Seq.filter (fun point -> 
    // check if x-mas
    let diagonalOne = 
      [ point; point.Move(UpRight); point.Move(DownLeft) ]
      |> List.map _.Value
      |> Set.ofList
    let diagonalTwo = 
      [ point; point.Move(UpLeft); point.Move(DownRight) ]
      |> List.map _.Value
      |> Set.ofList
    diagonalOne = xmas && diagonalTwo = xmas)
  |> Seq.length

let sampleMatrix = Matrix(Path.Join(__SOURCE_DIRECTORY__, "example.txt"))
let matrix = Matrix(Path.Join(__SOURCE_DIRECTORY__, "input.txt")) 

partOne sampleMatrix
partOne matrix

partTwo sampleMatrix
partTwo matrix