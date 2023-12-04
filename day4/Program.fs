open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let debug_enabled = 
        args
        |> Seq.exists (fun arg -> arg = "--debug-part2")
    let input = read_stdin_lines() in
    let (part1, part2) = Puzzle.solve input debug_enabled
    printfn "Part 1: %A" part1
    printfn "Part 2: %A" part2 
    0
