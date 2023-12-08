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


module Pathfinding = begin
    type Direction = 
        | Left
        | Right
        with 
            static member parse = function
                | 'L' -> Left
                | 'R' -> Right
                | other -> failwith (sprintf "Invalid direction %A" other)

    type Instructions = {
        turns : Direction array
    }
        with
            static member parse (line : string) : Instructions =
                let turns = 
                    line
                    |> Seq.map Direction.parse
                    |> Array.ofSeq
                in
                { turns = turns }
                
            member this.turn_number (number : int) : Direction =
                let index = number % this.turns.Length in
                this.turns[index]

    type Node = {
        name : string
        left : string
        right : string
    }
        with 
            static member parse (line : string) : Node =
                match line with 
                    | RegEx @"(?<name>[0-9A-Z]+) = \((?<left>[0-9A-Z]+), (?<right>[0-9A-Z]+)\)" groups ->
                        let name = groups["name"].Value in
                        let left = groups["left"].Value in
                        let right = groups["right"].Value in
                        { name = name; left = left; right = right }
                    | _ -> failwith ("Invalid line: " + line)

                member this.turn = function
                    | Left -> this.left
                    | Right -> this.right


    type Network = {
        nodes : Map<string, Node>
    }
        with
            static member parse (lines : string seq) : Network =
                let nodes =
                    lines
                    |> Seq.map Node.parse
                    |> Seq.map (fun node -> 
                        (node.name, node)
                    )
                    |> Map.ofSeq
                in
                { nodes = nodes }

            member this.Item(node_name : string) : Node =
                this.nodes[node_name]

    type State = {
        map : Network
        instructions : Instructions
        step_counter : int
        current_positions : string array
    }
        with 
            static member parse (start_position_finder : Network -> string array) (lines : string seq) : State =
                let instructions = 
                    lines
                    |> Seq.head
                    |> Instructions.parse
                in
                let map = 
                    lines
                    |> Seq.skip 2
                    |> Network.parse
                in
                let start_positions = 
                    map |> start_position_finder
                in
                { 
                    map = map;
                    instructions = instructions
                    step_counter = 0 
                    current_positions = start_positions
                }

            member this.step() : State =
                let turn = this.instructions.turn_number (this.step_counter) in
                let next_positions = [|
                    for position in this.current_positions do
                        yield this.map[position].turn turn
                |] in
                { this with
                        step_counter = this.step_counter + 1
                        current_positions = next_positions
                }


end

module Puzzle = begin
    open Pathfinding
    let part1 (input: string seq) =
        let start_with_AAA = (fun _ -> [|"AAA"|])
        let mutable state = State.parse start_with_AAA input in
        while state.current_positions[0] <> "ZZZ" do
            state <- state.step()
        done;
        state.step_counter

    let part2 (input: string seq) =
        // This seems to be one of those ones where the naive approach will take forever, and clever optimization is
        //  needed. 
        //
        // Thinking through the problem statement, if we have _several_ positions, and all of them can be "arrived" 
        // at the same point in time, then it must mean that we can safely move off of an end position knowing that
        // we'll come back to it -- and if we're going to come back to it, that means we're going to loop.
        //
        // So the trick is: find out how many steps each start-position's loop is from start to finish, and then 
        //  grab the least-common-multiple of those numbers (so that we have 1 complete longest loop, and N complete 
        //  shorter loops).
        //
        // I probably won't hae time to implement that today, but I'm leaving a comment for future me to implement it
        // so I don't forget this train of thought.
        // 
        // Below this line was my naive solution:
        // -------------------------------------------
        let start_with_theAs = (fun (network : Network) -> 
            network.nodes.Keys
            |> Seq.filter (fun node_name -> 
                node_name.EndsWith 'A'
            )
            |> Array.ofSeq
        )
        let all_ghosts_have_arrived (state : State) : bool =
            state.current_positions
            |> Seq.forall (fun position -> position.EndsWith 'Z')
        let mutable state = Pathfinding.State.parse start_with_theAs input in
        while not (all_ghosts_have_arrived state) do
            state <- state.step()
        done;
        state.step_counter
end
