def getOptionOrThrow(option : Option[a]) : a =
    match option {
        case Some(v) => v
        case None => bug!("Expected option to be non-empty")
    }

mod ElfGame {
    pub type alias Supplies = {
        reds = Int64,
        greens = Int64,
        blues = Int64
    }

    pub def parse_round(roundStr : String) : Supplies =
        let segments = 
            roundStr
            |> String.split(regex = ",\\s*");

        def getMarblesOfColor(color : String) : Int64 =
            segments
            |> List.find(String.contains(substr = color))
            |> Option.flatMap(segment -> 
                segment
                |> String.trim
                |> String.takeWhile(Char.isDigit)
                |> Int64.fromString
            )
            |> Option.getWithDefault(0i64);

        {
            reds = getMarblesOfColor("red"), 
            blues = getMarblesOfColor("blue"), 
            greens = getMarblesOfColor("green")
        }

    pub def power(supplies : Supplies) : Int64 =
        supplies.reds * supplies.greens * supplies.blues

    pub type alias Game = {
        id = Int32,
        rounds = List[Supplies]
    }
    pub def parse_game(line : String) : Game =
        let (gameIdPart, roundsStr) =
            match String.split(regex = ":", line) {
                case gameIdPart :: roundsStr :: Nil => (gameIdPart, roundsStr)
                case _ => bug!("Invalid line: ${line}")
            };
        let gameId : Int32 = 
            gameIdPart
            |> String.dropWhile(d -> not Char.isDigit(d))
            |> String.takeWhile(Char.isDigit)
            |> Int32.fromString
            |> getOptionOrThrow;
        let result = { 
            id = gameId, 
            rounds = 
                roundsStr
                |> String.split(regex = ";")
                |> List.map(ElfGame.parse_round)
        };
        result

    pub def can_be_played_with(supplies : Supplies, game : Game) : Bool =
        game.rounds
        |> List.forAll(round ->
            round.reds <= supplies.reds and
            round.greens <= supplies.greens and
            round.blues <= supplies.blues
        )

    pub def minimum_supplies_required(game : Game) : Supplies =
        let (minReds, minGreens, minBlues) = 
            game.rounds
            |> List.foldLeft(
                    (accumulator, round) ->
                        let (minReds, minGreens, minBlues) = accumulator;
                        (
                            Int64.max(minReds, round.reds),
                            Int64.max(minGreens, round.greens),
                            Int64.max(minBlues, round.blues)
                        ), 
                    (0i64, 0i64, 0i64)
                );
        { reds = minReds, blues = minBlues, greens = minGreens }
}

def part1(input : List[String]) : Int64 =
    let bag : ElfGame.Supplies = {
        reds = 12i64, greens = 13i64, blues = 14i64
    };
    let games =
        input
        |> List.filter(line -> not String.isEmpty(line))
        |> List.map(ElfGame.parse_game);

    games
    |> List.filter(ElfGame.can_be_played_with(bag))
    |> List.map(game -> game.id)
    |> List.map(Int32.toInt64)
    |> List.foldLeft(Add.add, 0i64)

def part2(input : List[String]) : Int64 =
    let games =
        input
        |> List.filter(line -> not String.isEmpty(line))
        |> List.map(ElfGame.parse_game);

    games
    |> List.map(ElfGame.minimum_supplies_required)
    |> List.map(ElfGame.power)
    |> List.foldLeft(Add.add, 0i64)

def main() : Unit \ IO =
    let lines = region rc {
        Iterator.toList(System.StdIn.readLines(rc))
    };
    println("Part 1: ${part1(lines)}");
    println("Part 2: ${part2(lines)}")