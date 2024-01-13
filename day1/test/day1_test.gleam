import gleam/list 
import gleeunit
import gleeunit/should
import lib

pub fn main() {
  gleeunit.main()
}

pub fn part1_test() {
	"pqr3stu8vwx"
	|> lib.line_to_number(lib.numeric_digit_finder)
	|> should.equal(38)
}

pub fn part2_test() {
	let input_data = [
		#("two1nine", 29),
		#("eightwothree", 83),
		#("abcone2threexyz", 13),
		#("xtwone3four", 24),
		#("4nineeightseven2", 42),
		#("zoneight234", 14),
		#("7pqrstsixteen", 76)
	]
	input_data
	|> list.map(fn (test_case) {
		let #(input, expected_result) = test_case
		let actual_result = 
			input
			|> lib.line_to_number(lib.word_or_digit_finder)

		actual_result |> should.equal(expected_result)

		actual_result
	})
	|> list.fold(from: 0, with: fn(a, b) { a + b })
}
