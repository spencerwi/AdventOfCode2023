import gleam/list
import gleeunit
import gleeunit/should
import lib

pub fn main() {
  gleeunit.main()
}

pub fn parse_game_test() {
	"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
	|> lib.parse_game
	|> should.equal(lib.Game(
		round: 3,
		supplies_required: lib.Supplies(
			reds: 20,
			greens: 13,
			blues: 6
		)
	))
}

pub fn can_play_test() {
	let input_supplies = lib.Supplies(
		reds: 12,
		greens: 13,
		blues: 14
	)
	let input_lines = [
		"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
		"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
		"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
		"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
		"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
	]
	input_lines
	|> list.map(lib.parse_game)
	|> list.filter(fn(game) { lib.can_play(game, with: input_supplies )})
	|> list.map(fn(game) { game.round })
	|> should.equal([1, 2, 5])

}

