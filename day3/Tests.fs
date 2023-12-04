module Tests
open Lib
open type Schematic
open type Coords
open type PartNumber

open NUnit.Framework
open FsUnit

let sample_input_raw = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

let sample_input = sample_input_raw.Trim().Split "\n"

let uneven_input_raw = """
.1234.
.$....
....2.
"""
let uneven_input = uneven_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for Schematic`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()= 
        Schematic.parse sample_input
        |> should equal {
            cells = (array2D [|
                [| '4'; '6'; '7'; '.'; '.'; '1'; '1'; '4'; '.'; '.' |]
                [| '.'; '.'; '.'; '*'; '.'; '.'; '.'; '.'; '.'; '.'; |];
                [| '.'; '.'; '3'; '5'; '.'; '.'; '6'; '3'; '3'; '.'; |];
                [| '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'; |];
                [| '6'; '1'; '7'; '*'; '.'; '.'; '.'; '.'; '.'; '.'; |];
                [| '.'; '.'; '.'; '.'; '.'; '+'; '.'; '5'; '8'; '.'; |];
                [| '.'; '.'; '5'; '9'; '2'; '.'; '.'; '.'; '.'; '.'; |];
                [| '.'; '.'; '.'; '.'; '.'; '.'; '7'; '5'; '5'; '.'; |];
                [| '.'; '.'; '.'; '$'; '.'; '*'; '.'; '.'; '.'; '.'; |];
                [| '.'; '6'; '6'; '4'; '.'; '5'; '9'; '8'; '.'; '.'; |]
            |]) 
        }

    [<Test>]
    member this.``It should have correct width and height`` ()=
        let schematic = Schematic.parse uneven_input in
        schematic.width |> should equal 6;
        schematic.height |> should equal 3

    [<Test>]
    member this.``It should look up individual indexes correctly`` ()=
        let schematic = Schematic.parse uneven_input in
        schematic[0, 1] |> should equal '1'

    [<Test>]
    member this.``It should find symbols correctly`` ()= 
        let schematic = Schematic.parse uneven_input in
        schematic.symbol_locations
        |> should equal (seq {
            { row = 1; col = 1 }
        })
                
[<TestFixture>] 
type ``Tests fo r solution`` ()=
    [<Test>]    
    member this .``It should solve part 1`` ()=
        Puzzle. part1 sample_input
        |> should equal 4361
                
    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal (int64 467835)
