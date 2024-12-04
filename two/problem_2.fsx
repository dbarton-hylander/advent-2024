open System
open System.Collections.Generic
open System.IO

let inputPath = Path.Join(__SOURCE_DIRECTORY__, "input.txt")

let reports = List<int list>()

let reader = new StreamReader(inputPath)
let mutable line = reader.ReadLine()
while line <> null do
  let trimmed = line.Trim()
  if not <| String.IsNullOrEmpty trimmed then 
    let report = 
      trimmed.Split(" ")
      |> Array.map Int32.Parse
      |> List.ofArray
    reports.Add report
  line <- reader.ReadLine()
    
reader.Dispose()

type Direction = Up | Down | Unknown
type Accumulator = int option * Direction * int

let checkReport report =
  match 
    List.windowed 2 report
    |> List.fold (fun (acc: Accumulator) windowPair -> 
      let diff = windowPair[1] - windowPair[0]
      
      let direction = if diff > 0 then Up else Down
      let previousFailure, currentDir, idx = acc
      
      let first = 
        if currentDir <> Unknown && currentDir <> direction then 
          previousFailure |> Option.orElse (Some idx)
        elif Math.Abs diff > 0 && Math.Abs diff <= 3 then 
          previousFailure |> Option.orElse None
        else
          previousFailure |> Option.orElse (Some idx)

      first, direction, idx + 1
    ) (None, Unknown, 0) with
  | failureIdx, _, _ -> failureIdx

let checkReportWithRetry report =
  let rec removeAtIdx (maybeRemoveIdx: int option) = 
    let reportToProcess = 
      maybeRemoveIdx
      |> Option.map (fun idx -> report |> List.removeAt idx)
      |> Option.defaultValue report
    let nextIdx = 
      maybeRemoveIdx 
      |> Option.map (fun i -> i + 1)
      |> Option.defaultValue 0
    match checkReport reportToProcess with
    | Some failureIdx -> 
      if (nextIdx < report.Length) then 
        removeAtIdx (Some nextIdx)
      else false
    | None -> true
  removeAtIdx None
  
// part 1
reports 
|> List.ofSeq
|> List.filter (fun report ->
  match checkReport report with
  | Some _ -> false
  | _ -> true)
|> List.length

// part 2
reports 
|> List.ofSeq
|> List.filter checkReportWithRetry
|> List.length

// answer = 366
