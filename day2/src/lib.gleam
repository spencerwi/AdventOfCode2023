import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Supplies {
	Supplies(reds: Int, greens: Int, blues: Int)
}
pub fn available_supplies_satisfies_requirements(
	available supplies: Supplies, 
	requirements requirements: Supplies
) -> Bool {
	supplies.reds >= requirements.reds &&
	supplies.greens >= requirements.greens &&
	supplies.blues >= requirements.blues
}

pub fn parse_supplies(from input : String) -> Supplies {
	let segments = string.split(input, on: ", ")
	let find_marble_count = fn(label : String) -> Int {
		segments 
		|> list.find(fn (segment) { string.ends_with(segment, label) })
		|> result.try(fn (segment) { 
			segment
			|> string.replace(each: label, with: "")
			|> string.trim
			|> int.parse
		})
		|> result.unwrap(or: 0)
	}
	Supplies(
		reds: find_marble_count("red"),
		greens: find_marble_count("green"),
		blues: find_marble_count("blue")
	)
}

pub type Game {
	Game(round: Int, supplies_required: Supplies)
}

pub fn parse_game(from input: String) -> Game {
	let assert [id_label, cube_groups_str] = string.split(input, on: ": ")
	let assert Ok(id) = 
		id_label
		|> string.replace(each: "Game ", with: "")
		|> int.parse
	let cube_groups = string.split(cube_groups_str, on:"; ")
	let supply_groups = 
		cube_groups
		|> list.map(parse_supplies)

	let assert supplies_required = 
		supply_groups
		|> list.fold(from: Supplies(0, 0, 0), with: fn(current, acc) {
			Supplies(
				reds: int.max(current.reds, acc.reds),
				greens: int.max(current.greens, acc.greens),
				blues: int.max(current.blues, acc.blues)
			)
		})

	Game(
		round: id,
		supplies_required: supplies_required
	)
}

pub fn power_of(game: Game) -> Int {
	game.supplies_required.reds * game.supplies_required.greens * game.supplies_required.blues
}

pub fn can_play(game game: Game, with supplies: Supplies) -> Bool {
	available_supplies_satisfies_requirements(
		available: supplies,
		requirements: game.supplies_required
	)
}

