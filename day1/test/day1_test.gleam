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
	"zoneight234"
	|> lib.line_to_number(lib.word_or_digit_finder)
	|> should.equal(14)

	"7pqrstsixteen"
	|> lib.line_to_number(lib.word_or_digit_finder)
	|> should.equal(76)
}
