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
        grid.creature_start |> should equal {x = 1; y = 1}
        Map.toSeq grid.pipes |> should equivalent [
            ({x = 1; y = 1}, TopLeft)
            ({x = 2; y = 1}, Horizontal)
            ({x = 3; y = 1}, TopRight)
            ({x = 1; y = 2}, Vertical)
            ({x = 3; y = 2}, Vertical)
            ({x = 1; y = 3}, BottomLeft)
            ({x = 2; y = 3}, Horizontal)
            ({x = 3; y = 3}, BottomRight)
        ]
        grid.ToString() |> should equal (String.concat "\n" input_lines)

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
        grid.connecting_pipes_for {x = 1; y = 1}
        |> should equivalent [
            {x = 1; y = 2}
            {x = 2; y = 1}
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
