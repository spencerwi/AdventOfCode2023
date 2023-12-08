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
                    | RegEx @"(?<name>[A-Z]+) = \((?<left>[A-Z]+), (?<right>[A-Z]+)\)" groups ->
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
        current_position : string
    }
        with 
            static member parse (lines : string seq) : State =
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
                { 
                    map = map;
                    instructions = instructions
                    step_counter = 0 
                    current_position = "AAA"
                }

            member this.step() : State =
                let turn = this.instructions.turn_number (this.step_counter) in
                let next = this.map[this.current_position].turn turn in
                { this with
                        step_counter = this.step_counter + 1
                        current_position = next
                }


end

module Puzzle = begin
    let part1 (input: string seq) =
        let mutable state = Pathfinding.State.parse input in
        while state.current_position <> "ZZZ" do
            state <- state.step()
        done;
        state.step_counter + 0

    let part2 (input: string seq) =
        "the right answer"
end
