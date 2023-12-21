module Tests
open Lib
open System

open NUnit.Framework
open FsUnit

let sample_input_raw = 
    """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....""" |> _.Trim()

let sample_input = sample_input_raw.Split "\n"

[<TestFixture>]
type ``Tests for OuterSpace`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()=
        let space = OuterSpace.parse sample_input
        space.galaxies |> should equivalent [
            {row = 0; col = 3};
            {row = 1; col = 7};
            {row = 2; col = 0};
            {row = 4; col = 6};
            {row = 5; col = 1};
            {row = 6; col = 9};
            {row = 8; col = 7};
            {row = 9; col = 0};
            {row = 9; col = 4}
        ]

    [<Test>]
    member this.``It should generate all pairs of galaxies correctly`` ()=
        let space = OuterSpace.parse sample_input
        let pairs = space.allPairs() in
        Seq.length pairs |> should equal 36

    [<Test>]
    member this.``It should measure distance between two points accurately`` ()=
        let space = OuterSpace.parse sample_input
        space.space_distance_between space.galaxies[4] space.galaxies[8]
        |> should equal 9

        

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 374

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal "the right answer"
