import gleam/int
import gleam/iterator
import gleam/list
import gleam/result
import gleam/string

pub fn list_index_of(list : List(a), item: a) -> Result(Int, Nil) {
	iterator.from_list(list)
	|> iterator.index
	|> iterator.find(fn (word) { item == word.0 })
	|> result.map(fn (found) {
		found.1
	})
}

pub fn string_index_of(haystack : String, substring: String) -> Int {
	case haystack {
		"" -> -1
		_other -> {
			case string.starts_with(haystack, substring) {
				True -> 0
				False -> {
					1 + string_index_of(
						string.drop_left(haystack, up_to: 1), 
						substring
					)
				}
			}
		}
	}
}


pub const digits : String = "0123456789"
pub fn is_digit(x : String) -> Bool {
	string.contains(does: digits, contain: x)
}

pub const word_numbers = [
	"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
]

pub fn word_to_digit(word: String) -> Int {
	case list_index_of(word_numbers, word) {
		Ok(idx) -> idx + 1
		_ -> panic as {"Invalid number-word: " <> word}
	}
}

pub type DigitFinder = 
	fn(String) -> #(Int, Int)

pub fn numeric_digit_finder(line: String) -> #(Int, Int) {
	let digits = 
		string.to_graphemes(line)
		|> list.filter(is_digit)
	let assert Ok(first_str) = list.first(digits)
	let assert Ok(last_str) = list.last(digits)
	let assert Ok(first) = int.parse(first_str)
	let assert Ok(last) = int.parse(last_str)
	#(first, last)
}

pub fn word_or_digit_finder(line: String) -> #(Int, Int) {
	let digit_strs = string.to_graphemes(digits)
	let all_search_terms = list.concat([digit_strs, word_numbers])
	let occurrences = 
		iterator.from_list(all_search_terms)
		|> iterator.map(fn (term) {
			#(term, string_index_of(line, term), string_index_of(string.reverse(line), string.reverse(term)))
		})
		|> iterator.filter(fn (match) { match.1 > -1 })
		|> iterator.to_list

	let assert Ok(#(first_match, _, _)) = 
		occurrences
		|> list.sort(by: fn(a, b) { int.compare(a.1, b.1) })
		|> list.first
	let assert Ok(#(last_match, _, _)) = 
		occurrences
		|> list.sort(by: fn(a,b) { int.compare(a.2, b.2) })
		|> list.first

	let to_int = fn(term) {
		case is_digit(term) {
			True -> {
				let assert Ok(i) = int.parse(term)
				i
			}
			False -> {
				word_to_digit(term)
			}
		}
	}

	#(to_int(first_match), to_int(last_match))
}


pub fn line_to_number(line: String, digit_finder : DigitFinder) -> Int {
	let #(first_digit, last_digit) = 
		line
		|> digit_finder
	{first_digit * 10} + last_digit
}
