module Tests
open Lib

open NUnit.Framework
open FsUnit

let sample_input_raw = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for Oasis`` ()=
    [<Test>]
    member this.``It can generate a differences array`` ()=
        let numbers = Oasis.parse_numbers sample_input[0] in
        Oasis.to_differences numbers 
        |> should equalSeq [
            3; 3; 3; 3; 3
        ]

    [<Test>]
    member this.``It can extrapolate for sample data`` ()=
        sample_input
        |> Seq.map Oasis.parse_numbers
        |> Seq.map Oasis.extrapolate
        |> should equalSeq [
            18; 28; 68
        ]

    [<Test>]
    member this.``It can extrapolate backwards for sample data`` ()=
        sample_input
        |> Seq.map Oasis.parse_numbers
        |> Seq.map Oasis.extrapolate_backwards
        |> should equalSeq [
            -3; 0; 5
        ]


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 114

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 2
