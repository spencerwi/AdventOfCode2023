module Tests
open Lib
open CardGame
open type Hand
open type Player

open NUnit.Framework
open FsUnit

let sample_input_raw = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for Card`` ()=
    [<Test>]
    member this.``It should parse correctly`` ()=
        "123456789TJQKA" 
        |> Seq.map Card.parse
        |> Seq.toList
        |> should equal [
            Number 1
            Number 2
            Number 3
            Number 4
            Number 5
            Number 6
            Number 7
            Number 8
            Number 9
            Number 10
            JackOrJoker
            Queen
            King
            Ace
        ]

    [<Test>]
    member this.``It should reject invalid cards`` ()=
        (fun () -> ignore <| Card.parse '0')
        |> should (throwWithMessage "Invalid card: '0'") typeof<System.Exception>
        (fun () -> ignore <| Card.parse 'Z')
        |> should (throwWithMessage "Invalid card: 'Z'") typeof<System.Exception>

    [<Test>]
    member this.``It should implement value correctly for Jacks`` ()=
        "123456789TJQKA"
        |> Seq.map Card.parse
        |> Seq.map (fun card -> card.value Jacks)
        |> should equalSeq [
            1
            2
            3
            4
            5
            6
            7
            8
            9
            10
            11
            12
            13
            14
        ]

    [<Test>]
    member this.``It should implement value correctly for Jokers`` ()=
        "123456789TJQKA"
        |> Seq.map Card.parse
        |> Seq.map (fun card -> card.value Jokers)
        |> should equalSeq [
            1
            2
            3
            4
            5
            6
            7
            8
            9
            10
            -1
            12
            13
            14
        ]


[<TestFixture>]
type ``Tests for HandType`` ()=
    member this.``It should have ordering correctly`` ()=
        FiveOfAKind |> should be (greaterThan FourOfAKind)
        FourOfAKind |> should be (greaterThan FullHouse)
        FullHouse |> should be (greaterThan ThreeOfAKind)
        ThreeOfAKind |> should be (greaterThan TwoPair)
        TwoPair |> should be (greaterThan OnePair)
        OnePair |> should be (greaterThan HighCard)

[<TestFixture>]
type ``Tests for Hand``  ()=

    [<Test>]
    member this.``It should parse correctly`` ()=
        let hand = CardGame.Hand.parse "32T3K" in
        hand.cards |> should equalSeq [|
            Number 3
            Number 2
            Number 10
            Number 3
            King
        |]


    [<Test>]
    member this.``It should score correctly with Jacks`` ()=
        let test_cases = [
                ("AAAAA", FiveOfAKind)
                ("AAAA1", FourOfAKind)
                ("AAA11", FullHouse)
                ("AAA12", ThreeOfAKind)
                ("AA122", TwoPair)
                ("12AA3", OnePair)
                ("12345", HighCard)
            ]
        in
        for (input, expected_score) in test_cases do
            let hand = Hand.parse input in
            hand.score_with_jacks() |> should equal expected_score

    [<Test>]
    member this.``It should score correctly with Jokers`` ()=
        let test_cases = [
                ("AJAAA", FiveOfAKind)
                ("JAAA1", FourOfAKind)
                ("AAJ11", FullHouse)
                ("JAA12", ThreeOfAKind)
                ("AJ122", ThreeOfAKind)
                ("12AJ3", OnePair)
                ("J2345", OnePair)
            ]
        in
        for (input, expected_score) in test_cases do
            let hand = Hand.parse input in
            hand.score_with_jokers() |> should equal expected_score



[<TestFixture>]
type ``Tests for Player`` ()= 
    [<Test>]
    member this.``It should parse correctly`` ()=
        sample_input
        |> Seq.map Player.parse
        |> should equalSeq [
            { bet = 765; hand = Hand.parse "32T3K" }
            { bet = 684; hand = Hand.parse "T55J5" }
            { bet = 28;  hand = Hand.parse "KK677" }
            { bet = 220; hand = Hand.parse "KTJJT" }
            { bet = 483; hand = Hand.parse "QQQJA" }
        ]

[<TestFixture>]
type ``Tests for CardGame`` ()=
    [<Test>]
    member this.``It should rank players correctly with jacks`` ()=
        let disordered = [
            "AAAAA"
            "KKKKK"
            "AAAA1"
            "1AAAA"
            "22AAA"
            "11AAA"
            "21AAA"
            "12AAA"
            "33122"
            "22133"
            "33124"
            "12433"
            "54321"
            "12345"
        ] in
        disordered
        |> Seq.mapi (fun idx hand -> {
            hand = Hand.parse hand;
            bet = idx
        })
        |> CardGame.rank_players CardGame.ScoringMode.Jacks
        |> Seq.map _.hand
        |> should equalSeq [
            Hand.parse "12345"
            Hand.parse "54321"
            Hand.parse "12433"
            Hand.parse "33124"
            Hand.parse "22133"
            Hand.parse "33122"
            Hand.parse "12AAA"
            Hand.parse "21AAA"
            Hand.parse "11AAA"
            Hand.parse "22AAA"
            Hand.parse "1AAAA"
            Hand.parse "AAAA1"
            Hand.parse "KKKKK"
            Hand.parse "AAAAA"
        ]


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 6440

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 5905
