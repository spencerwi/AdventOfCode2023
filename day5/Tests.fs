module Tests
open Lib

open NUnit.Framework
open FsUnit

let sample_input_raw = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for RangeMapping`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()=
        let input = "50 98 2" in
        RangeMapping.parse input 
        |> should equal {
            destination = 50
            source = 98
            length = 2
        }

    [<Test>]
    member this.``It should be able to identify numbers as being within the source range or not`` ()=
        let mapping = RangeMapping.parse "50 98 2" in
        mapping.contains 98 |> should equal true;
        mapping.contains 50 |> should equal false;
        mapping.contains 100 |> should equal false;

    [<Test>]
    member this.``It should map numbers in the source range properly`` ()=
        let mapping = RangeMapping.parse "50 98 2" in
        mapping.map_value 98 |> should equal 50;
        mapping.map_value 99 |> should equal 51;

    [<Test>]
    member this.``It should leave numbers outside source range alone`` ()=
        let mapping = RangeMapping.parse "50 98 2" in
        mapping.map_value 42 |> should equal 42;
        mapping.map_value 100 |> should equal 100

    [<Test>]
    member this.``It should reverse correctly`` ()=
        let mapping = RangeMapping.parse "50 98 2" in
        mapping.reversed() |> should equal { destination = 98; source = 50; length = 2};
        mapping.reversed().map_value 50 |> should equal 98

[<TestFixture>]
type ``Tests for Garden`` ()=
    [<Test>]
    member this.``It should parse and map seeds correctly`` ()=
        let garden = Garden.parse sample_input in
        garden.map_seeds_to_locations() 
        |> should equal [82L; 43L; 86L; 35L]

    [<Test>]
    member this.``It should run a given value in reverse properly`` ()=
        let garden = Garden.parse sample_input in
        [82L; 43L; 86L; 35L]
        |> List.map garden.run_value_in_reverse
        |> should equal [79L; 14L; 55L; 13L]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 35

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 46
