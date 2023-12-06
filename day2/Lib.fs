module Lib
open System
open System.Text.RegularExpressions

/// A RegEx active pattern for easier regexing
let (|RegEx|_|) p i =
    let m = Regex.Match(i, p) in
    if m.Success then
        Some m.Groups
    else
        None

module ElfGame = begin
    type Supplies = {
        reds: int;
        greens: int
        blues: int
    } with
        static member parse_round (round_str : string) : Supplies =
            let mutable reds = 0 in
            let mutable greens = 0 in
            let mutable blues = 0 in
            round_str.Split(", ")
            |> Array.iter (fun group -> 
                match group with 
                | RegEx "(?<count>\d+) red" g -> 
                    reds <- (g["count"].Value |> int)
                | RegEx "(?<count>\d+) blue" g ->
                    blues <- (g["count"].Value |> int)
                | RegEx "(?<count>\d+) green" g ->
                    greens <- (g["count"].Value |> int)
                | _ -> ()
            )
            { reds = reds; greens = greens; blues = blues }

        member this.power : int =
            this.reds * this.greens * this.blues


    type Game = {
        id: int;
        rounds : Supplies array
    }
        with 
            static member parse (line : string) : Game =
                let [|game_id_part; rounds_str|] = line.Split ":" in
                let game_id = 
                    match game_id_part with
                    | RegEx "Game (?<game_id>\d+)" g ->
                        g["game_id"].Value |> int
                    | _ -> failwith ("Invalid game ID part: " + game_id_part)
                in
                { id = game_id; rounds = (rounds_str.Split ";") |> Array.map Supplies.parse_round }

            member this.can_be_played_with (supplies : Supplies) =
                this.rounds 
                |> Seq.forall (fun round ->
                    round.reds <= supplies.reds &&
                    round.greens <= supplies.greens &&
                    round.blues <= supplies.blues
                )

            member this.minimum_supplies_required () = 
                let mutable min_reds = 0 in
                let mutable min_greens = 0 in
                let mutable min_blues = 0 in
                for round in this.rounds do
                    if round.reds > min_reds then min_reds <- round.reds
                    if round.greens > min_greens then min_greens <- round.greens
                    if round.blues > min_blues then min_blues <- round.blues
                done;
                { reds = min_reds; greens = min_greens; blues = min_blues }

end

module Puzzle = begin
    let part1 (input: string seq) =
        let bag : ElfGame.Supplies = {
            reds = 12; greens = 13; blues = 14
        } in
        let games = 
            input 
            |> Seq.filter (not << String.IsNullOrWhiteSpace)
            |> Seq.map ElfGame.Game.parse
        in 
        games
            |> Seq.filter (fun game -> game.can_be_played_with bag)
            |> Seq.map _.id
            |> Seq.sum

    let part2 (input: string seq) =
        let games = 
            input 
            |> Seq.filter (not << String.IsNullOrWhiteSpace)
            |> Seq.map ElfGame.Game.parse
        in 
        games
            |> Seq.map _.minimum_supplies_required()
            |> Seq.map _.power
            |> Seq.sum
end
