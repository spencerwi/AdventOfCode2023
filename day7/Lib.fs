module Lib
open System

module CardGame = begin
    type ScoringMode =
        | Jacks
        | Jokers

    type Card =
        | Number of int
        | JackOrJoker
        | Queen
        | King
        | Ace
    with 
        member this.value (scoring_mode : ScoringMode) = 
            match this with
            | Number n -> n
            | JackOrJoker when scoring_mode = Jacks -> 11
            | JackOrJoker when scoring_mode = Jokers -> -1
            | Queen -> 12
            | King -> 13
            | Ace -> 14

        static member parse = function
            | n when n > '0' && n <= '9' -> Number (n |> string |> int)
            | 'T' -> Number 10
            | 'J' -> JackOrJoker
            | 'Q' -> Queen
            | 'K' -> King
            | 'A' -> Ace
            | other -> failwith (sprintf "Invalid card: %A" other)

    type HandType =
        | HighCard
        | OnePair
        | TwoPair
        | ThreeOfAKind
        | FullHouse
        | FourOfAKind
        | FiveOfAKind

    type Hand = {
        cards : Card array
    }
        with 
            static member parse (input : string) : Hand =
                if (input.Trim().Length) > 5 then
                    failwith (sprintf "Invalid hand \"%s\": Hands can only have 5 cards" (input.Trim()))
                let cards = 
                    input
                    |> Seq.map Card.parse
                    |> Array.ofSeq
                in 
                { 
                    cards = cards 
                }

            member this.score_with_jacks() =
                let counts_of_each_card = 
                    this.cards
                    |> Seq.countBy id
                in 
                let highest_count = 
                    counts_of_each_card
                    |> Seq.maxBy (fun (card, count) -> count)
                in
                match highest_count with
                | (_, 5) -> FiveOfAKind
                | (_, 4) -> FourOfAKind
                | (card, 3) -> 
                    // is this a full house or just three-of-a-kind?
                    let maybePair = 
                        counts_of_each_card
                        |> Seq.tryFind (fun (card, count) -> count = 2)
                    in
                    match maybePair with
                    | Some (other_card, _) -> FullHouse 
                    | None -> ThreeOfAKind 
                | (card, 2) -> 
                    let pairs = 
                        counts_of_each_card
                        |> Seq.filter (fun (card, count) -> count = 2)
                        |> List.ofSeq
                    in
                    if pairs.Length = 2 then
                        TwoPair 
                    else
                        OnePair
                | _ -> 
                    HighCard 

            member this.score_with_jokers() =
                let non_jacks = 
                    this.cards
                    |> Seq.filter (fun card -> card <> JackOrJoker)
                    |> Seq.toList
                in
                match non_jacks with
                | [] -> FiveOfAKind
                | _ -> 
                    let card_counts = 
                        non_jacks
                        |> Seq.countBy id
                    in
                    let best_target_card = 
                        card_counts
                        |> Seq.maxBy (fun (card, count) -> (count, card.value Jacks))
                        |> fst
                    let new_hand_cards = [|
                        for card in this.cards do
                            if card = JackOrJoker then
                                yield best_target_card
                            else
                                yield card
                    |]
                    in
                    let new_hand = { 
                        cards = new_hand_cards
                    } in
                    new_hand.score_with_jacks()
                        

    type Player = {
        bet : int
        hand : Hand
    }
        with
            static member parse (input : string) =
                match input.Split() with
                | [|hand_str; bet_str|] ->
                    {
                        bet = int bet_str
                        hand = Hand.parse hand_str
                    }
                | _ -> failwith (sprintf "Invalid round line: %s" input)

    let rank_players (scoring_mode : ScoringMode) (players : Player seq) : Player seq =
        let scoring_rule = 
            match scoring_mode with
            | Jacks -> _.hand.score_with_jacks()
            | Jokers -> _.hand.score_with_jokers()
        in
        players 
        |> Seq.sortWith (fun player1 player2 ->
            match (scoring_rule player1, scoring_rule player2) with
            | (a, b) when a > b -> 1
            | (a, b) when a < b -> -1
            | _ ->
                Seq.zip player1.hand.cards player2.hand.cards
                |> Seq.map (fun (player1_card, player2_card) -> 
                    let card1_value = player1_card.value scoring_mode in
                    let card2_value = player2_card.value scoring_mode in
                    compare card1_value card2_value
                )
                |> Seq.tryFind ((<>) 0)
                |> Option.defaultValue 0
        )

end

module Puzzle = begin
    let play_game (scoring_mode : CardGame.ScoringMode) (players : CardGame.Player seq) : int =
        players
        |> CardGame.rank_players scoring_mode
        |> Seq.mapi (fun index player ->
            let rank = index + 1 in // zero-based indexing
            rank * player.bet
        )
        |> Seq.sum

    let part1 (input: string seq) =
        input
        |> Seq.map CardGame.Player.parse
        |> play_game CardGame.ScoringMode.Jacks

            

    let part2 (input: string seq) =
        input
        |> Seq.map CardGame.Player.parse
        |> play_game CardGame.ScoringMode.Jokers
end
