module Tests
open Lib

open NUnit.Framework
open FsUnit

let sample_input_raw = """.....
.S-7.
.|.|.
.L-J.
....."""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for parsing pipes`` ()=
    [<Test>]
    member this.``It should handle a simple case correctly`` ()=
        let input_lines = [|
            "....."
            ".S-7."
            ".|.|."
            ".L-J."
            "....."
        |] in
        let grid = Grid.parse input_lines in
        grid.creature_start |> should equal {col = 1; row = 1}
        grid.ToString() |> should equal <| String.concat "\n" input_lines

    [<Test>]
    member this.``It should handle a complex case correctly`` ()=
        let input = """......
7-F7-.
.FJ|7.
SJLL7.
|F--J.
LJ.LJ.
......"""
        in
        let grid = Grid.parse (input.Split "\n") in
        grid.ToString() |> should equal input

[<TestFixture>]
type ``Tests for Grid`` ()=
    [<Test>]
    member this.``It should be able to find connecting pipes for a given cell`` ()=
        let input_lines = [|
            "....."
            ".S-7."
            ".|.|."
            ".L-J."
            "....."
        |] in
        let grid = Grid.parse input_lines in
        grid.connecting_pipes_for {col = 1; row = 1}
        |> should equivalent [
            (South, {col = 1; row = 2})
            (East, {col = 2; row = 1})
        ]




[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 4

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal "the right answer"
