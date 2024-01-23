import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/erlang
import lib

pub fn read_all_stdin() -> List(String) {
	case erlang.get_line("") {
		Ok(line) -> [string.trim(line), ..read_all_stdin()]
		_ -> []
	}
}

pub fn part1(supplies available: lib.Supplies, games games: List(lib.Game)) -> Int {
	games 
	|> list.filter(fn (game) {
		lib.can_play(game, with: available)
	})
	|> list.map(fn (game) { game.round })
	|> list.fold(from: 0, with: int.add)
}

pub fn part2(games: List(lib.Game)) -> Int {
	games
	|> list.map(lib.power_of)
	|> list.fold(from: 0, with: int.add)
}

pub fn main() {
	let input = read_all_stdin()
	let games = 
		input
		|> list.map(lib.parse_game)
	io.println("Part 1: " <> int.to_string(part1(
		supplies: lib.Supplies(reds: 12, greens: 13, blues: 14),
		games: games
	)))
	io.println("Part 2: " <> int.to_string(part2(games)))
}
