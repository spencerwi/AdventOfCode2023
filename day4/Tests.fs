module Tests
open Lib
open type LottoCard

open NUnit.Framework
open FsUnit

let sample_input_raw = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for LottoCard`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()=
        LottoCard.parse sample_input[0]
        |> should equal {
            card_id = 1
            winning_numbers = Set.ofList [41; 48; 83; 86; 17]
            my_numbers = Set.ofList [83; 86; 6; 31; 17; 9; 48; 53]
        }

    [<Test>]
    member this.``It should score a card correctly`` ()=
        let card = LottoCard.parse sample_input[1] in
        card.score |> should equal 2

[<TestFixture>]
type ``Tests for LottoGame`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()=
        let game = LottoGame.parse sample_input in
        game.cards.Length |> should equal sample_input.Length


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 (LottoGame.parse sample_input)
        |> should equal 13

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 (LottoGame.parse sample_input) true
        |> should equal 30

    [<Test>]
    member this.``It should invoke the right answers for solve`` ()=
        let expected_part1 = Puzzle.part1 (LottoGame.parse sample_input) in
        let expected_part2 = Puzzle.part2 (LottoGame.parse sample_input) false in
        Puzzle.solve sample_input false
        |> should equal (expected_part1, expected_part2)
