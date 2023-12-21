module Lib
open System

type Coords = {
    row: int
    col: int
}
    with 
        member this.manhattan_distance_to (other : Coords) : int =
            if this = other then 0
            else
                abs (other.col - this.col) +
                abs (other.row - this.row)

type OuterSpace = {
    galaxies: Coords list
}
    with 
        static member parse (input : string array) =
            let galaxies = [
                for row in 0 .. (input.Length - 1) do
                    let line = input[row] in
                    for col in 0 .. (line.Length - 1) do
                        if line[col] = '#' then 
                            yield {row = row; col = col}
            ]
            in 
            {galaxies = galaxies}

        member this.is_row_empty (row : int) : bool = 
            this.galaxies
            |> Seq.exists (fun g -> g.row = row)
            |> not

        member this.is_col_empty (col : int) : bool = 
            this.galaxies
            |> Seq.exists (fun g -> g.col = col)
            |> not

        member this.allPairs() : (Coords * Coords) seq =
            Seq.allPairs this.galaxies this.galaxies
            |> Seq.distinctBy (fun (a, b) -> 
                [a; b] |> List.sort // (a, b) and (b, a) are the same; keep a predictable ordering to ensure distinct works
            )
            |> Seq.filter (fun (a, b) -> a <> b)

        // naive thought: manhattan distance, but then add in extra steps for wide rows and columns
        // This gives a wrong answer even on sample input. I probably should switch to something like A*
        member this.space_distance_between (a : Coords) (b : Coords) : int =
            if a = b then 0 
            else
                let extras (a_val : int) (b_val : int) (empty_checker : int -> bool) =
                    let (smaller, bigger) =
                        if a_val < b_val then (a_val, b_val)
                        else (b_val, a_val)
                    in
                    seq {smaller .. bigger}
                    |> Seq.filter empty_checker
                    |> Seq.length
                in
                a.manhattan_distance_to b +
                (extras a.row b.row this.is_row_empty) +
                (extras a.col b.col this.is_col_empty)


module Puzzle = begin
    let part1 (input: string array) =
        let space = OuterSpace.parse input in
        Seq.allPairs space.galaxies space.galaxies
        |> Seq.distinct
        |> Seq.map (fun (a, b) -> space.space_distance_between a b)
        |> Seq.sum


    let part2 (input: string seq) =
        "the right answer"
end
