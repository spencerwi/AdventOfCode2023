module Lib
open System
open System.Text.RegularExpressions
open System.Collections.Generic

/// A RegEx active pattern for easier regexing
let (|RegEx|_|) p i =
    let m = Regex.Match(i, p) in
    if m.Success then
        Some m.Groups
    else
        None

type LottoCard = {
    card_id : int
    winning_numbers : int Set
    my_numbers : int Set
}
    with
        static member parse (input_line : string) : LottoCard =
            match input_line with 
            | RegEx @"Card\s+(?<card_id>\d+):\s+(?<winning_numbers>\d+\s*)+\s+\|\s+(?<my_numbers>\d+\s*)+" groups ->
                let card_id = int (groups["card_id"].Value) in
                let winning_numbers = 
                    groups["winning_numbers"].Captures
                    |> Seq.map (fun capture -> int <| capture.Value.Trim())
                    |> Set.ofSeq
                in
                let my_numbers = 
                    groups["my_numbers"].Captures
                    |> Seq.map (fun capture -> int <| capture.Value.Trim())
                    |> Set.ofSeq
                in
                { card_id = card_id; winning_numbers = winning_numbers; my_numbers = my_numbers }
            | _ -> failwith ("Invalid line: " + input_line)

        member this.matching_numbers : int =
            Set.intersect this.winning_numbers this.my_numbers
            |> Set.count

        member this.score : int = 
            if this.matching_numbers = 0 then 
                0
            else
                2 |> pown <| (this.matching_numbers - 1)

type LottoGame = {
    cards : LottoCard array
}
    with 
        static member parse (input_lines : string seq) =
            { cards =
                input_lines
                |> Seq.map LottoCard.parse
                |> Seq.toArray
            }

        member this.score_simple : int =
            this.cards
            |> Seq.map _.score
            |> Seq.sum

        member this.score_complex (debug : bool) : int =
            let mutable cards_seen = [] in
            let mutable queue : Queue<LottoCard> = new Queue<LottoCard>(this.cards);
            while queue.Count > 0 do
                let current_card = queue.Dequeue() in
                cards_seen <- current_card :: cards_seen;
                let cards_won = current_card.matching_numbers in
                if debug then printfn "From card %d, we won %d more cards" current_card.card_id cards_won 
                if cards_won > 0 then
                    // also enqueue the cards we just won
                    for won_card_index in current_card.card_id .. current_card.card_id + cards_won - 1 do
                        if won_card_index < this.cards.Length then
                            let won_card = this.cards[won_card_index] in
                            if debug then printfn "  ...grabbing another one of card %d" won_card.card_id;
                            queue.Enqueue(won_card)
                    done
            done;
            if debug then begin
                let counts_of_each_card = 
                    cards_seen
                    |> Seq.countBy _.card_id
                    |> Map.ofSeq;
                printfn "We ended up with %d cards; here's the breakdown of how many of each card we have: %A" (List.length cards_seen) counts_of_each_card;
            end;
            List.length cards_seen

module Puzzle = begin
    let part1 (game: LottoGame) =
        game.score_simple

    let part2 (game: LottoGame) (debug : bool) =
        game.score_complex debug

    let solve (input: string seq) (debug_part2 : bool) =
        let game = LottoGame.parse input in
        (part1 game, game.score_complex debug_part2)

end
