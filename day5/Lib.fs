module Lib
open System
open FSharp.Collections.ParallelSeq

type Range = {
    start : int64
    stop : int64
}
    with
        static member inclusive (start : int64) (stop : int64) : Range =
            { start = start; stop = stop }

        static member exclusive (start : int64) (stop : int64) : Range =
            { start = start; stop = stop - 1L }

        member this.contains (n : int64) =
            n >= this.start && n <= this.stop

type RangeMapping = {
    destination: int64
    source: int64
    length: int64
}
    with 
        static member parse (line: string) : RangeMapping =
            match line.Split " " |> Array.map int64 with
            | [|destination; source; length|] -> 
                { 
                    destination = destination
                    source = source
                    length = length 
                }
            | _ -> failwith ("Invalid line: " + line)

        member this.contains (value : int64) : bool =
            value >= this.source &&
            value < (this.source + this.length)
    
        member this.map_value (value : int64) : int64 =
            if this.contains value then
                let offset = value - this.source in
                this.destination + offset
            else
                value

        member this.reversed() : RangeMapping =
            { this with
                    source = this.destination
                    destination = this.source
            }

type Garden = {
    seeds: int64 list
    seed_to_soil: RangeMapping list
    soil_to_fertilizer: RangeMapping list
    fertilizer_to_water: RangeMapping list
    water_to_light: RangeMapping list
    light_to_temperature: RangeMapping list
    temperature_to_humidity: RangeMapping list
    humidity_to_location: RangeMapping list
}
    with 
        static member parse (input_lines : string array) =
            let seeds = 
                input_lines[0].Replace("seeds: ", "").Split(" ")
                |> Seq.map int64
                |> List.ofSeq
            in
            let get_mappings_with_header_line (header : string) : RangeMapping list =
                input_lines 
                |> Seq.skipWhile (fun line -> not <| line.StartsWith(header))
                |> Seq.skip 1
                |> Seq.takeWhile (not << String.IsNullOrWhiteSpace)
                |> Seq.map RangeMapping.parse
                |> List.ofSeq

            let seed_to_soil = get_mappings_with_header_line "seed-to-soil" in
            let soil_to_fertilizer = get_mappings_with_header_line "soil-to-fertilizer" in
            let fertilizer_to_water = get_mappings_with_header_line "fertilizer-to-water" in
            let water_to_light = get_mappings_with_header_line "water-to-light" in
            let light_to_temperature = get_mappings_with_header_line "light-to-temperature" in
            let temperature_to_humidity = get_mappings_with_header_line "temperature-to-humidity" in
            let humidity_to_location = get_mappings_with_header_line "humidity-to-location" in
            {
                seeds = seeds
                seed_to_soil = seed_to_soil
                soil_to_fertilizer = soil_to_fertilizer
                fertilizer_to_water = fertilizer_to_water
                water_to_light = water_to_light
                light_to_temperature = light_to_temperature
                temperature_to_humidity = temperature_to_humidity
                humidity_to_location = humidity_to_location
            }


        member this.map_seeds_to_locations() : int64 seq =
            let apply_range_group (group : RangeMapping list) (initial_value : int64) : int64 =
                let maybe_applicable_mapping = 
                    group
                    |> Seq.tryFind (fun mapping -> mapping.contains initial_value)
                in
                match maybe_applicable_mapping with
                | Some m -> m.map_value initial_value
                | None -> initial_value
            in
            this.seeds 
            |> Seq.map (apply_range_group this.seed_to_soil)
            |> Seq.map (apply_range_group this.soil_to_fertilizer)
            |> Seq.map (apply_range_group this.fertilizer_to_water)
            |> Seq.map (apply_range_group this.water_to_light)
            |> Seq.map (apply_range_group this.light_to_temperature)
            |> Seq.map (apply_range_group this.temperature_to_humidity)
            |> Seq.map (apply_range_group this.humidity_to_location)

        member this.run_value_in_reverse (value : int64) : int64 =
            let apply_reversed_group (group : RangeMapping list) (initial_value : int64) : int64 =
                let maybe_applicable_mapping =
                    group
                    |> Seq.map _.reversed()
                    |> Seq.tryFind (fun reversed_mapping -> reversed_mapping.contains initial_value)
                in
                match maybe_applicable_mapping with
                | Some m -> m.map_value initial_value
                | None -> initial_value
            in
            value
            |> apply_reversed_group this.humidity_to_location
            |> apply_reversed_group this.temperature_to_humidity
            |> apply_reversed_group this.light_to_temperature
            |> apply_reversed_group this.water_to_light
            |> apply_reversed_group this.fertilizer_to_water
            |> apply_reversed_group this.soil_to_fertilizer
            |> apply_reversed_group this.seed_to_soil

        member this.seeds_as_ranges() : Range list =
            this.seeds
            |> Seq.chunkBySize 2
            |> Seq.map (fun [|start; length|] -> Range.exclusive start (start + length))
            |> List.ofSeq


module Puzzle = begin
    let part1 (input: string seq) =
        let garden = Garden.parse (Seq.toArray input) in
        let locations = garden.map_seeds_to_locations() in
        printfn "Seeds %A mapped to locations %A" garden.seeds locations;
        Seq.min locations

    let part2 (input: string seq) =
        let garden = Garden.parse (Seq.toArray input) in
        let seed_ranges : Range list = garden.seeds_as_ranges() in
        let infiniteLongs = seq {
            let mutable i = 0L in
            while true do
                yield i
                i <- i + 1L
            done
        } in
        infiniteLongs
        |> PSeq.find (fun x ->
            let result = garden.run_value_in_reverse (int64 x) in
            PSeq.exists (fun (range : Range) -> range.contains result) seed_ranges
        )

end
