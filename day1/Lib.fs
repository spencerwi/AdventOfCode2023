module Lib

open System
open System.Text.RegularExpressions

module Calibration = begin
    let numeric_digit_finder (line : string) = 
        let digits = 
            line
            |> Seq.filter Char.IsDigit
            |> Seq.map (string >> int)
        in
        (Seq.head digits, Seq.last digits)

    let word_numbers = [|
        "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"
    |]

    let word_to_digit (word : string) = 
        match Array.IndexOf(word_numbers, word) with
        | -1 -> failwith ("Unrecognized digit-word: " + word)
        | d -> d + 1

    let word_or_number_digit_finder (line : string) =
        let occurrences = seq {
            for word in word_numbers do
                let first_index = line.IndexOf word in
                if first_index > -1 then
                    yield (word_to_digit word, first_index, line.LastIndexOf word)

            for digit in 0 .. 9 do
                let first_index = line.IndexOf (string digit) in
                if first_index > -1 then
                    yield (digit, first_index, line.LastIndexOf (string digit))
        } in
        let first_digit = 
            occurrences
            |> Seq.minBy (fun (_, first, _) -> first)
            |> (fun (digit, _, _) -> digit)
        in
        let last_digit =
            occurrences
            |> Seq.maxBy (fun (_, _, last) -> last)
            |> (fun (word, _, _) -> word)
        in
        (first_digit, last_digit)


    let line_to_number (digit_finder : string -> int * int) (line : string) =
        let (first_digit, last_digit) = digit_finder line in
        (first_digit * 10) + last_digit

end

module Puzzle = begin
    let part1_line (line : string) =
        line 
        |> Calibration.line_to_number Calibration.numeric_digit_finder

    let part2_line (line: string) =
        line 
        |> Calibration.line_to_number Calibration.word_or_number_digit_finder

    let solve (input: string seq) =
        input 
        |> Seq.filter (not << String.IsNullOrWhiteSpace)
        |> Seq.map (fun line -> (part1_line line, part2_line line))
        |> Seq.fold (fun (part1_sum, part2_sum) (part1_num, part2_num) -> 
            (part1_sum + part1_num, part2_sum + part2_num)
        ) (0, 0)

end
