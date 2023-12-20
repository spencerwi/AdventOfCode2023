module Lib
open System.Collections.Generic

type Direction = 
    | North
    | South
    | East
    | West

    with 
        static member values() : Direction seq =
            seq { 
                North
                South
                East
                West
            }

        member this.reversed() : Direction =
            match this with
            | North -> South
            | South -> North
            | East -> West
            | West -> East

type Coords = {
    col: int
    row: int
}
    with 
        member this.move = function
            | North -> { this with row = this.row - 1}
            | South -> { this with row = this.row + 1}
            | East -> { this with col = this.col + 1}
            | West -> { this with col = this.col - 1}


type PipeShape = 
    | Vertical 
    | Horizontal
    | BottomLeft
    | BottomRight
    | TopRight
    | TopLeft
    with 
        static member tryParse = function
            | '|' -> Some Vertical
            | '-' -> Some Horizontal
            | 'L' -> Some BottomLeft
            | 'J' -> Some BottomRight
            | '7' -> Some TopRight
            | 'F' -> Some TopLeft
            | other -> None

        override this.ToString() : string =
            match this with
            | Vertical -> "|"
            | Horizontal -> "-"
            | BottomLeft -> "L"
            | BottomRight -> "J"
            | TopRight -> "7"
            | TopLeft -> "F"
        

        static member values() : PipeShape seq =
            seq {
                Vertical ; Horizontal ; BottomLeft ; BottomRight ; TopRight ; TopLeft
            }

        member this.connects_to = function
            | North when this = Vertical -> true
            | South when this = Vertical -> true
            | East when this = Horizontal -> true
            | West when this = Horizontal -> true
            | North when this = BottomLeft -> true
            | East when this = BottomLeft -> true
            | North when this = BottomRight -> true
            | West when this = BottomRight -> true
            | South when this = TopLeft -> true
            | East when this = TopLeft -> true
            | South when this = TopRight -> true
            | West when this = TopRight -> true
            | _ -> false

type Cell = 
    | Pipe of PipeShape
    | EmptySpace 
    with 
        override this.ToString() : string =
            match this with
            | Pipe p -> p.ToString()
            | EmptySpace -> "."


type Grid = {
    cells : Cell[,]
    creature_start : Coords
}
    with 
        static member parse (input : string array) =
            let mutable creature_start = None in
            let mutable cells = 
                array2D [|
                    for y in 0 .. (input.Length - 1) do
                        let line = input[y] in
                        yield [|
                            for x in 0 .. (line.Length - 1) do
                                let coords = {col = x; row = y} in
                                match line[x] with
                                | '.' -> yield EmptySpace
                                | 'S' -> 
                                    creature_start <- Some coords
                                    yield EmptySpace
                                | other -> 
                                    match PipeShape.tryParse other with
                                    | Some pipeShape -> yield (Pipe pipeShape)
                                    | other -> failwith $"Invalid space at {coords}: {other}"
                        |]
                |]
            in
            let result_without_creature_replaced = {
                cells = cells;
                creature_start = creature_start.Value
            }
            // Now replace the creature's start point with the appropriate pipe
            let creature_start_pipe_connections = 
                result_without_creature_replaced.connecting_pipes_for creature_start.Value
            in
            let creature_start_pipe_shape =
                PipeShape.values()
                |> Seq.find (fun candidate_shape ->
                    creature_start_pipe_connections
                    |> Seq.forall (fun (direction, _) -> candidate_shape.connects_to direction)
                )
            in
            cells[creature_start.Value.row, creature_start.Value.col] <- Pipe creature_start_pipe_shape;
            {cells = cells; creature_start = creature_start.Value}

        member this.Item(coords : Coords) = this.cells[coords.row, coords.col]
        member this.TryGetPipe (coords : Coords) = 
            if coords.col < 0 || coords.col >= this.width || coords.row < 0 || coords.row >= this.height then
                None
            else
                match this[coords] with 
                | Pipe pipeShape -> Some pipeShape
                | _ -> None

        member this.width : int =
            Array2D.length2 this.cells

        member this.height : int =
            Array2D.length1 this.cells

        member this.connecting_pipes_for (coords : Coords) : (Direction * Coords) seq =
            seq {
                for direction_from_coords in Direction.values() do
                    let candidate_neighbor = coords.move direction_from_coords in
                    match this.TryGetPipe candidate_neighbor with
                    | Some neighbor_pipe -> 
                        let direction_from_neighbor = direction_from_coords.reversed() in
                        if neighbor_pipe.connects_to direction_from_neighbor then
                            yield (direction_from_coords, candidate_neighbor)
                    | _ -> ()
            }           


        override this.ToString() : string =
            seq {
                for y in 0 .. (this.height - 1) do
                    for x in 0 .. (this.width - 1) do
                        if x = this.creature_start.col && y = this.creature_start.row then
                            yield "S"
                        else
                            yield this[{col = x; row = y}].ToString()
                    done;
                    yield "\n"
            } 
            |> String.concat ""
            |> _.Trim()

        /// <description>
        /// Using Dijkstra's algorithm recycled from AoC 2021 day 15 and AOC 2022 day 12, because why figure this out from scratch again?
        /// </description>
        member this.distances_from_start =
            let mutable distancesFromSource = Map.ofSeq [
                (this.creature_start, 0)
            ] 
            in
            // Now, we want to walk from the source "outwards" and trace each shortest path. 
            // Sometimes, we'll see a cell again but on a shorter path. No worries, dawg, just 
            // update its distance-from-the-source and go check it out again to see if anything's 
            // changed about it.
            let unsettled = new Queue<Coords>([this.creature_start]) in
            while unsettled.Count > 0 do
                let current = unsettled.Dequeue() in
                for (_, neighbor) in (this.connecting_pipes_for current) do
                    let distanceThroughCurrent = distancesFromSource[current] + 1 in
                    let existingDistance = 
                        match distancesFromSource.TryFind neighbor with
                        | Some n -> n
                        | None -> distanceThroughCurrent + 10000
                    in
                    if distanceThroughCurrent < existingDistance then begin
                        distancesFromSource <- Map.add neighbor distanceThroughCurrent distancesFromSource
                        unsettled.Enqueue neighbor
                    end
                done
            done;
            distancesFromSource


module Puzzle = begin
    let part1 (input: string array) =
        let grid = Grid.parse input in
        grid.distances_from_start
        |> Map.values
        |> Seq.max

    let part2 (input: string seq) =
        "the right answer"
end
