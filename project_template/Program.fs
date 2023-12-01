open System
open Lib 

let read_stdin_lines () : string seq =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    printfn "Part 1: %s" (Puzzle.part1 input);
    printfn "Part 2: %s" (Puzzle.part2 input);
    0
