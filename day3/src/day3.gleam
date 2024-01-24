import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/set
import gleam/string
import gleam/erlang
import lib/schematic

pub fn read_all_stdin() -> List(String) {
	case erlang.get_line("") {
		Ok(line) -> [string.trim(line), ..read_all_stdin()]
		_ -> []
	}
}

pub fn part1(lines: List(String)) -> Int {
	lines
	|> schematic.parse
	|> schematic.part_numbers
	|> set.to_list
	|> list.map(fn (part_number) { part_number.value })
	|> list.fold(from: 0, with: int.add)
}

pub fn part2(lines: List(String)) -> Int {
	lines
	|> schematic.parse
	|> schematic.gear_ratios
	|> iterator.fold(from: 0, with: int.add)
}

pub fn main() {
	let input = read_all_stdin()
	io.println("Part 1: " <> int.to_string(part1(input)))
	io.println("Part 2: " <> int.to_string(part2(input)))
}
