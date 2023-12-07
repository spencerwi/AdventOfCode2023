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
    member this.``It should parse correctly with Jacks`` ()=
        "123456789TJQKA" 
        |> Seq.map (Card.parse DeckType.Jacks)
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
            Jack
            Queen
            King
            Ace
        ]

    member this.``It should parse correctly with Jokers`` ()=
        "123456789TJQKA" 
        |> Seq.map (Card.parse DeckType.Jokers)
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
            Joker
            Queen
            King
            Ace
        ]


    [<Test>]
    member this.``It should reject invalid cards`` ()=
        (fun () -> ignore <| Card.parse Jacks '0')
        |> should (throwWithMessage "Invalid card: '0'") typeof<System.Exception>
        (fun () -> ignore <| Card.parse Jacks 'Z')
        |> should (throwWithMessage "Invalid card: 'Z'") typeof<System.Exception>

    [<Test>]
    member this.``It should implement value correctly for Jacks`` ()=
        "123456789TJQKA"
        |> Seq.map (Card.parse DeckType.Jacks)
        |> Seq.map _.value
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
        Joker.value |> should equal -1


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
        let hand = CardGame.Hand.parse DeckType.Jacks  "32T3K" in
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
            let hand = Hand.parse DeckType.Jacks input in
            hand.score() |> should equal expected_score

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
            let hand = Hand.parse DeckType.Jokers input in
            hand.score() |> should equal expected_score



[<TestFixture>]
type ``Tests for Player`` ()= 
    [<Test>]
    member this.``It should parse correctly`` ()=
        sample_input
        |> Seq.map (Player.parse DeckType.Jacks)
        |> should equalSeq [
            { bet = 765; hand = Hand.parse DeckType.Jacks "32T3K" }
            { bet = 684; hand = Hand.parse DeckType.Jacks "T55J5" }
            { bet = 28;  hand = Hand.parse DeckType.Jacks "KK677" }
            { bet = 220; hand = Hand.parse DeckType.Jacks "KTJJT" }
            { bet = 483; hand = Hand.parse DeckType.Jacks "QQQJA" }
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
            hand = Hand.parse DeckType.Jacks hand;
            bet = idx
        })
        |> CardGame.rank_players 
        |> Seq.map _.hand.ToString()
        |> should equalSeq [
            "12345"
            "54321"
            "12433"
            "33124"
            "22133"
            "33122"
            "12AAA"
            "21AAA"
            "11AAA"
            "22AAA"
            "1AAAA"
            "AAAA1"
            "KKKKK"
            "AAAAA"
        ]

    [<Test>]
    member this.``It should rank players correctly with Jokers`` ()=
        sample_input
        |> Seq.map (Player.parse DeckType.Jokers)
        |> CardGame.rank_players
        |> Seq.map _.hand.ToString()
        |> should equalSeq [
            "32T3K"
            "KK677"
            "T55J5"
            "QQQJA"
            "KTJJT"
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
