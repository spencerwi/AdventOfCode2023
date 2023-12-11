module Tests
open Lib
open System

open NUnit.Framework
open FsUnit

let sample_input_raw = """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 2

    [<Test>]
    member this.``It should solve part 2`` ()=
        let part_2_input_raw = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)""" 
        let part_2_input = 
            part_2_input_raw.Trim().Split "\n"
        in
        Puzzle.part2 part_2_input
        |> should equal 6L
