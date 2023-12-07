module Lib
open System

module CardGame = begin
    type DeckType =
        | Jacks
        | Jokers

    type Card =
        | Joker
        | Number of int
        | Jack
        | Queen
        | King
        | Ace
    with 
        member this.value = 
            match this with
            | Number n -> n
            | Jack -> 11
            | Joker -> -1
            | Queen -> 12
            | King -> 13
            | Ace -> 14

        static member parse (deck_type : DeckType) = function
            | n when n > '0' && n <= '9' -> Number (n |> string |> int)
            | 'T' -> Number 10
            | 'J' when deck_type = Jacks -> Jack
            | 'J' when deck_type = Jokers -> Joker
            | 'Q' -> Queen
            | 'K' -> King
            | 'A' -> Ace
            | other -> failwith (sprintf "Invalid card: %A" other)

        member this.to_char = 
            match this with
            | Number 10 -> 'T'
            | Number n -> n |> string |> Seq.head
            | Jack | Joker -> 'J'
            | Queen -> 'Q'
            | King -> 'K'
            | Ace -> 'A'

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
            static member parse (deck_type : DeckType) (input : string) : Hand =
                if (input.Trim().Length) > 5 then
                    failwith (sprintf "Invalid hand \"%s\": Hands can only have 5 cards" (input.Trim()))
                let cards = 
                    input
                    |> Seq.map (Card.parse deck_type)
                    |> Array.ofSeq
                in 
                { 
                    cards = cards 
                }

            member this.contains_jokers : bool =
                this.cards
                |> Seq.exists ((=) Joker)

            member this.convert_jokers() : Hand =
                let non_jokers = 
                    this.cards
                    |> Seq.filter (fun card -> card <> Joker)
                    |> Seq.toList
                in
                if non_jokers.IsEmpty then
                    { cards = [|Ace;Ace;Ace;Ace;Ace|] } // Dude, you gotta have a poker face, like me. WOAH, FIVE ACES!
                else
                    let card_counts = 
                        non_jokers
                        |> Seq.countBy id
                    in
                    let best_target_card = 
                        card_counts
                        |> Seq.maxBy (fun (card, count) -> (count, card.value))
                        |> fst
                    in
                    let new_hand_cards = [|
                        for card in this.cards do
                            if card = Joker then
                                yield best_target_card
                            else
                                yield card
                    |]
                    in
                    { cards = new_hand_cards }

            member this.score() =
                if this.contains_jokers then
                    this.convert_jokers().score()
                else
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
                        
            override this.ToString() : string =
                this.cards
                |> Seq.map _.to_char
                |> String.Concat

                        

    type Player = {
        bet : int
        hand : Hand
    }
        with
            static member parse (deck_type : DeckType) (input : string) =
                match input.Split() with
                | [|hand_str; bet_str|] ->
                    {
                        bet = int bet_str
                        hand = Hand.parse deck_type hand_str
                    }
                | _ -> failwith (sprintf "Invalid round line: %s" input)

    let rank_players (players : Player seq) : Player seq =
        players 
        |> Seq.sortBy (fun player -> 
            let card_values = 
                player.hand.cards
                |> Seq.map _.value
                |> Seq.toList
            in
            (player.hand.score(), card_values)
        )
end

module Puzzle = begin
    open CardGame
    let play_game (players : Player seq) : int =
        players
        |> CardGame.rank_players 
        |> Seq.mapi (fun index player ->
            let rank = index + 1 in // zero-based indexing
            rank * player.bet
        )
        |> Seq.sum

    let part1 (input: string seq) =
        input
        |> Seq.map (Player.parse DeckType.Jacks)
        |> play_game 

    let part2 (input: string seq) =
        input
        |> Seq.map (Player.parse DeckType.Jokers)
        |> play_game 
end
