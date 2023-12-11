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
                North;
                South;
                East;
                West
            }

        member this.reversed() : Direction =
            match this with
            | North -> South
            | South -> North
            | East -> West
            | West -> East

type Coords = {
    x: int
    y: int
}
    with 
        member this.move = function
            | North -> { this with y = this.y - 1}
            | South -> { this with y = this.y + 1}
            | East -> { this with x = this.x + 1}
            | West -> { this with x = this.x - 1}

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

type Grid = {
    pipes : Map<Coords, PipeShape>
    creature_start : Coords
}
    with 
        static member parse (input : string array) =
            let mutable creature_start = None in
            let mutable pipes = 
                seq {
                    for y in 0 .. (input.Length - 1) do
                        let line = input[y] in
                        for x in 0 .. (line.Length - 1) do
                            let coords = {x = x; y = y} in
                            match line[x] with
                            | 'S' -> 
                                creature_start <- Some coords
                            | other -> 
                                match PipeShape.tryParse other with
                                | Some pipeShape -> yield (coords, pipeShape)
                                | _ -> ()
                } |> Map.ofSeq
            in
            // Now replace the creature's start point with the appropriate pipe
            let creature_start_pipe_connections = seq {
                for direction_from_creature in Direction.values() do
                    let candidate_neighbor = creature_start.Value.move direction_from_creature in
                    if pipes.ContainsKey candidate_neighbor then
                        let neighbor_pipe = pipes[candidate_neighbor] in
                        let direction_from_neighbor = direction_from_creature.reversed() in
                        if neighbor_pipe.connects_to direction_from_neighbor then
                            yield direction_from_creature
            }
            in
            let creature_start_pipe_shape =
                PipeShape.values()
                |> Seq.find (fun candidate_shape ->
                    creature_start_pipe_connections
                    |> Seq.forall (fun direction -> candidate_shape.connects_to direction)
                )
            in
            pipes <- Map.add creature_start.Value creature_start_pipe_shape pipes;
            {pipes = pipes; creature_start = creature_start.Value}

        member this.Item(coords : Coords) = this.pipes[coords]
        member this.ContainsKey(coords: Coords) = this.pipes.ContainsKey coords
        member this.Keys = this.pipes.Keys
        member this.Values = this.pipes.Values

        member this.width : int =
            1 + (
                this.Keys
                |> Seq.map _.x
                |> Seq.max
            )

        member this.height : int =
            1 + (
                this.Keys
                |> Seq.map _.y
                |> Seq.max
            )

        member this.connecting_pipes_for (coords : Coords) : Coords seq =
            seq {
                for direction_from_coords in Direction.values() do
                    let candidate_neighbor = coords.move direction_from_coords in
                    if this.ContainsKey candidate_neighbor then
                        let neighbor_pipe = this[candidate_neighbor] in
                        let direction_from_neighbor = direction_from_coords.reversed() in
                        if neighbor_pipe.connects_to direction_from_neighbor then
                            yield candidate_neighbor
            }           


        override this.ToString() : string =
            seq {
                for y in 0 .. this.height do
                    for x in 0 .. this.width do
                        if x = this.creature_start.x && y = this.creature_start.y then
                            yield "S"
                        else
                            match this.pipes.TryFind {x = x; y = y} with
                            | None -> yield "."
                            | Some pipe -> yield pipe.ToString()
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
                for neighbor in (this.connecting_pipes_for current) do
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
