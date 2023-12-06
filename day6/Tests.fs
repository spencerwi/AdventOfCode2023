module Tests
open Lib
open type Race

open NUnit.Framework
open FsUnit

let sample_input_raw = """
Time:      7  15   30
Distance:  9  40  200
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for Race`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()=
        Race.parse_multiple sample_input
        |> should equal (seq {
            { time = 7; record_distance = 9};
            { time = 15; record_distance = 40};
            { time = 30; record_distance = 200}
        })

    [<Test>]
    member this.``It should count winning strategies correctly`` ()=
        let races = Race.parse_multiple sample_input in
        races 
        |> Seq.map (fun r -> r.winning_strategy_count)
        |> Seq.toList 
        |> should equal [4; 8; 9]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 288

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 71503
