module Tests
open Lib
open type ElfGame.Supplies
open type ElfGame.Game

open NUnit.Framework
open FsUnit
open System

let sample_input_raw = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

let sample_input = sample_input_raw.Split "\n" |> Seq.toArray

[<TestFixture>]
type ``Supplies tests`` ()=
    [<Test>]
    member this.``It should determine power correctly`` ()=
        let supplies = { reds = 4; greens = 2; blues = 6 } in
        supplies.power
        |> should equal 48


[<TestFixture>]
type ``ElfGame tests`` ()=
    [<Test>]
    member this.``It should parse a single game correctly`` ()=
        ElfGame.Game.parse sample_input.[1]
        |> should equal ({
            id = 1
            rounds = [|
                { blues = 3; reds = 4; greens = 0 };
                { reds = 1; greens = 2; blues = 6 };
                { greens = 2; reds = 0; blues = 0 }
            |]
        })

    [<Test>]
    member this.``It should evaluate playability correctly`` ()=
        let example_bag = {
            reds = 12
            greens = 13
            blues = 14
        } in
        let games = 
            sample_input
            |> Array.filter (not << String.IsNullOrWhiteSpace)
            |> Array.map ElfGame.Game.parse
        in
        games |> Array.map (fun game -> game.can_be_played_with example_bag)
        |> should equal [| true; true; false; false; true |]

    [<Test>]
    member this.``It should determine minimum supplies required correctly`` ()=
        let games = 
            sample_input
            |> Array.filter (not << String.IsNullOrWhiteSpace)
            |> Array.map ElfGame.Game.parse
        in
        games |> Array.map _.minimum_supplies_required()
        |> should equal [|
            { reds = 4; greens = 2; blues = 6 };
            { reds = 1; greens = 3; blues = 4 };
            { reds = 20; greens = 13; blues = 6 };
            { reds = 14; greens = 3; blues = 15 };
            { reds = 6; greens = 3; blues = 2}
        |]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 8

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 2286
