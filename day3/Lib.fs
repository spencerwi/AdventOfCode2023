module Lib
open System

type Coords = {
    row: int
    col: int
}
    with 
        member this.up() : Coords = { this with row = this.row - 1 }
        member this.down() : Coords = { this with row = this.row + 1 }
        member this.left() : Coords = { this with col = this.col - 1 }
        member this.right() : Coords = { this with col = this.col + 1 }

type PartNumber = {
    value : int
    left : Coords
    right : Coords
}

type Schematic = {
    cells : char[,]
}
    with 
        static member parse (lines : string seq) : Schematic =
            {
                cells = array2D lines
            }
        
        member this.Item(row : int, col : int) : char =
            this.cells[row, col]

        member this.Item(coords : Coords) : char =
            this.Item(coords.row, coords.col)

        member this.height : int =
            Array2D.length1 this.cells

        member this.width : int =
            Array2D.length2 this.cells

        member this.is_in_bounds(coords : Coords) =
            coords.row >= 0 && 
            coords.row < this.height &&
            coords.col >= 0 &&
            coords.col < this.width

        member this.neighbors_of(coords : Coords) : seq<Coords> =
            seq { 
                coords.up().left(); coords.up(); coords.up().right();
                coords.left(); coords.right(); 
                coords.down().left(); coords.down(); coords.down().right()
            } |> Seq.filter this.is_in_bounds

        member this.symbol_locations : seq<Coords> =
            seq {
                for row in 0..(this.height - 1) do
                    for col in 0..(this.width - 1) do
                        let value = this[row, col] in
                        if value <> '.' && not (Char.IsDigit value) then
                            yield {row = row; col = col}
            }

        member this.find_number_from (coords : Coords) : PartNumber option =
            if not (Char.IsDigit this[coords]) then
                None
            else
                let mutable start_of_number = coords in
                while this.is_in_bounds(start_of_number.left()) && Char.IsDigit this[start_of_number.left()] do
                    start_of_number <- start_of_number.left()
                done;
                let mutable end_of_number = coords in
                while this.is_in_bounds(end_of_number.right()) && Char.IsDigit this[end_of_number.right()] do
                    end_of_number <- end_of_number.right()
                done;
                let digits_in_order = [
                    for col in start_of_number.col .. end_of_number.col do
                        yield string this[coords.row, col]
                ]
                in
                let value = String.concat "" digits_in_order |> int
                Some {
                    value = value
                    left = start_of_number
                    right = end_of_number
                }

        member this.part_numbers : Set<PartNumber> =
            this.symbol_locations
            |> Seq.map this.neighbors_of
            |> Seq.concat
            |> Seq.map this.find_number_from
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Set.ofSeq

        member this.gear_ratios : seq<int64> =
            seq {
                for coords in this.symbol_locations do
                    if this[coords] = '*' then 
                        let neighboring_part_numbers = 
                            this.neighbors_of coords
                            |> Seq.map this.find_number_from
                            |> Seq.filter Option.isSome
                            |> Seq.map Option.get
                            |> Set.ofSeq
                        in
                        if (Set.count neighboring_part_numbers) = 2 then
                            match Set.toArray neighboring_part_numbers with
                            | [|one; two|] -> yield (int64 one.value) * (int64 two.value)
                            | _ -> ()
            }



module Puzzle = begin
    let part1 (input: string seq) =
        let schematic = Schematic.parse input in
        schematic.part_numbers
        |> Seq.map _.value
        |> Seq.sum

    let part2 (input: string seq) =
        let schematic = Schematic.parse input in
        schematic.gear_ratios
        |> Seq.sum
end
