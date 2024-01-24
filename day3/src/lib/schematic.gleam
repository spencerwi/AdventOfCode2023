import gleam/bool
import gleam/int
import gleam/iterator
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import lib/coords
import lib/part_number

pub type Schematic {
	Schematic(cells: List(List(String)))
}
pub fn parse(from lines: List(String)) -> Schematic {
	let cells = 
		lines
		|> list.map(string.trim)
		|> list.map(string.to_graphemes)

	Schematic(cells)
}

pub fn get(at coords: coords.Coords, from schematic: Schematic) -> Result(String, Nil) {
	let Schematic(cells) = schematic 
	cells
	|> list.at(coords.row)
	|> result.then(list.at(_, get: coords.col))
}

pub fn height(of schematic: Schematic) -> Int {
	list.length(schematic.cells)
}
pub fn width(of schematic: Schematic) -> Int {
	schematic.cells
	|> list.at(0)
	|> result.unwrap(or: [])
	|> list.length
}

pub fn is_in_bounds(is c: coords.Coords, within schematic: Schematic) -> Bool {
	c.row >= 0 &&
	c.col >= 0 &&
	c.row < height(of: schematic) &&
	c.col < width(of: schematic)
}

pub fn neighbors(of coords: coords.Coords, in schematic: Schematic) -> List(coords.Coords) {
	let assert [up, left, right, down] = [
		coords.up(from: coords),
		coords.left(from: coords),
		coords.right(from: coords),
		coords.down(from: coords)
	]
	let candidates = [
		coords.left(from: up), 		up, 		coords.right(from: up),
		left, 									right,
		coords.left(from: down), 	down, 		coords.right(from: down)
	]
	candidates
	|> list.filter(fn (neighbor) { is_in_bounds(is: neighbor, within: schematic) })
}

pub fn symbol_locations(in schematic: Schematic) -> iterator.Iterator(coords.Coords) {
	let rows = iterator.range(from: 0, to: height(schematic))
	let cols = iterator.range(from: 0, to: width(schematic))
	{
		use row <- iterator.flat_map(rows)
		use col <- iterator.map(cols)
		coords.Coords(row: row, col: col)
	}
}

pub fn is_digit(at coords: coords.Coords, in schematic: Schematic) -> Bool {
	let outcome = {
		use value <- result.map(get(coords, from: schematic))
		string.contains(does: "0123456789", contain: value)
	}
	result.unwrap(outcome, or: False)
}

pub fn find_part_number_from(start coords: coords.Coords, in schematic: Schematic) -> option.Option(part_number.PartNumber) {
	let outcome : Result(part_number.PartNumber, Nil) = {
		use value <- result.then(get(coords, from: schematic))
		use <- bool.guard(when: bool.negate(string.contains(does: "0123456789", contain: value)), return: Error(Nil))
		let left_digit_coords = 
			iterator.range(from: coords.col, to: 0)
			|> iterator.map(fn(col) { 
				coords.Coords(..coords, col: col)
			})
			|> iterator.take_while(is_digit(_, in: schematic))
			|> iterator.to_list
			|> list.reverse
		let left_digits = 
			left_digit_coords
			|> iterator.from_list
			|> iterator.map(get(at: _, from: schematic))
			|> iterator.map(fn (r) {
				let assert Ok(digit) = r
				digit
			})
			|> iterator.to_list

		let right_digit_coords =
			iterator.range(from:coords.col + 1, to: {width(schematic) - 1})
			|> iterator.map(fn(col) {
				coords.Coords(..coords, col: col)
			})
			|> iterator.take_while(is_digit(_, in: schematic))
			|> iterator.to_list
		let right_digits = 
			right_digit_coords
			|> iterator.from_list
			|> iterator.map(get(at: _, from: schematic))
			|> iterator.map(fn (r) {
				let assert Ok(digit) = r
				digit
			})
			|> iterator.to_list

		let all_digits = 
			list.concat([left_digits, right_digits])
			|> list.map(int.parse)
			|> list.map(fn(parse_result) { 
				let assert Ok(i) = parse_result 
				i
			})
		let assert Ok(numeric_value) = int.undigits(all_digits, 10)
		let assert Ok(leftmost) = list.first(left_digit_coords)
		let rightmost = case list.last(right_digit_coords) {
			Ok(r) -> r
			_ -> coords
		}
		Ok(
			part_number.PartNumber(
				value: numeric_value,
				left: leftmost,
				right: rightmost
			)
		)
	}
	option.from_result(outcome)
}

pub fn part_numbers(in schematic: Schematic) -> set.Set(part_number.PartNumber) {
	symbol_locations(in: schematic)
	|> iterator.map(neighbors(of: _, in: schematic))
	|> iterator.flat_map(iterator.from_list)
	|> iterator.map(find_part_number_from(start: _, in: schematic))
	|> iterator.to_list
	|> option.values
	|> set.from_list
}

pub fn gear_ratios(in schematic: Schematic) -> iterator.Iterator(Int) {
	symbol_locations(in: schematic)
	|> iterator.flat_map(fn (location) {
		case get(at: location, from: schematic) {
			Ok("*") -> {
				let neighboring_part_numbers = 
					neighbors(of: location, in: schematic)
					|> list.map(find_part_number_from(start: _, in: schematic))
					|> option.values
					|> set.from_list
				case set.to_list(neighboring_part_numbers) {
					[one, two] -> iterator.single(one.value * two.value)
					_ -> iterator.empty()
				}
			}
			_ -> iterator.empty()
		}
	})
}
