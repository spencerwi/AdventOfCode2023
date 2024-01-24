import lib/coords

pub type PartNumber {
	PartNumber(
		value: Int,
		left: coords.Coords,
		right: coords.Coords
	)
}
