pub type Coords {
	Coords(row: Int, col: Int)
}
pub fn up(from from: Coords) -> Coords {
	Coords(
		..from,
		row: from.row - 1
	)
}
pub fn down(from from: Coords) -> Coords {
	Coords(
		..from,
		row: from.row + 1
	)
}
pub fn left(from from: Coords) -> Coords {
	Coords(
		..from,
		col: from.col - 1
	)
}
pub fn right(from from: Coords) -> Coords {
	Coords(
		..from,
		col: from.col + 1
	)
}
