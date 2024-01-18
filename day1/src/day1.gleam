import gleam/int
import gleam/io
import gleam/iterator
import gleam/string

import gleam/erlang
import lib

pub fn read_all_stdin() -> List(String) {
	case erlang.get_line("") {
		Ok(line) -> [string.trim(line), ..read_all_stdin()]
		_ -> []
	}
}

pub fn part1(line: String) -> Int {
	lib.line_to_number(line, lib.numeric_digit_finder)
}
pub fn part2(line: String) -> Int {
	lib.line_to_number(line, lib.word_or_digit_finder)
}

pub fn solve(lines : List(String)) -> #(Int, Int) {
	lines
	|> iterator.from_list
	|> iterator.map(fn (line) { 
		let part2_result = part2(line)
		#(part1(line), part2_result)
	})
	|> iterator.fold(from: #(0, 0), with: fn(acc, current) {
		#(acc.0 + current.0, acc.1 + current.1)
	})
}

pub fn main() {
	let input = read_all_stdin()
	let #(part1, part2) = solve(input)
	io.println("Part 1: " <> int.to_string(part1))
	io.println("Part 2: " <> int.to_string(part2))
}
