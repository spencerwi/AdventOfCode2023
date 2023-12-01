open System
open Lib 

let read_stdin_lines () : string seq =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let (part1, part2) = (Puzzle.solve input) in
    printfn "Part 1: %d" part1;
    printfn "Part 2: %d" part2;
    0
