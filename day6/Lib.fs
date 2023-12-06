module Lib
open System

module Strings = begin
    let starts_with (prefix : string) (s : string) =
        s.StartsWith prefix

    let split_on_whitespace (s : string) = 
        s.Split() 

    let replace (find : string) (replace_with : string) (s : string) =
        s.Replace(find, replace_with)

    let find_line_with_prefix_and_unprefix_it (prefix : string) (input_lines : string seq) : string =
        input_lines
        |> Seq.find (starts_with prefix)
        |> replace prefix ""
end


type Race = {
    time : int64
    record_distance : int64
}
    with
        static member parse (numbers_parser : string -> int64 seq) (input_lines : string seq) : Race seq =
            let times = 
                input_lines
                |> Strings.find_line_with_prefix_and_unprefix_it "Time:"
                |> numbers_parser
            in
            let distances = 
                input_lines
                |> Strings.find_line_with_prefix_and_unprefix_it "Distance:"
                |> numbers_parser
            in
            Seq.zip times distances
            |> Seq.map (fun (time, distance) -> { time = time; record_distance = distance })

        static member parse_multiple (input_lines : string seq) : Race seq =
            input_lines
            |> Race.parse (fun numbers_line -> 
                numbers_line
                |> Strings.split_on_whitespace
                |> Seq.filter (not << String.IsNullOrWhiteSpace)
                |> Seq.map int64
            )

        static member parse_single (input_lines : string seq) : Race =
            input_lines
            |> Race.parse (fun numbers_line ->
                numbers_line
                |> Strings.replace " " ""
                |> int64
                |> Seq.singleton
            )
            |> Seq.head

        member this.winning_strategy_count : int =
            query { 
                for hold_time in 1L..(this.time - 1L) do
                    let speed = hold_time in
                    let time_remaining = this.time - hold_time in
                    let distance_traveled = speed * time_remaining in
                    where (distance_traveled > this.record_distance)
                    count
            }


module Puzzle = begin
    let part1 (input: string seq) =
        let races = Race.parse_multiple input in
        races
        |> Seq.map (fun race -> race.winning_strategy_count)
        |> Seq.fold ( * ) 1


    let part2 (input: string seq) =
        let race = Race.parse_single input in
        race.winning_strategy_count
end
