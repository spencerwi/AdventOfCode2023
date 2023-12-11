module Lib
open System

module Oasis = begin

    let parse_numbers (input : string) : int seq =
        input.Split()
        |> Seq.map int

    let is_all_zeroes (values : int seq) : bool =
        values
        |> Seq.forall ((=) 0)

    let to_differences (values : int seq) : int seq =
        values
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> b - a)

    let rec extrapolate (values : int seq) : int =
        if is_all_zeroes values then
            0
        else
            Seq.last values + extrapolate (to_differences values)

    let rec extrapolate_backwards (values : int seq) : int =
        if is_all_zeroes values then
            0
        else
            Seq.head values - extrapolate_backwards (to_differences values)

end

module Puzzle = begin
    let part1 (input: string seq) =
        input
        |> Seq.map Oasis.parse_numbers
        |> Seq.map Oasis.extrapolate
        |> Seq.sum

    let part2 (input: string seq) =
        input
        |> Seq.map Oasis.parse_numbers
        |> Seq.map Oasis.extrapolate_backwards
        |> Seq.sum
end
