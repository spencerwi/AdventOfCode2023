#!/bin/bash

if [ -z "$1" ] ; then
	echo "USAGE: newday.sh new NUMBER"
	exit 1
fi

if [ -d "day$1" ]; then
	echo "day$1 already exists."
	exit 1;
fi

gleam new --name "day$1" "day$1"
touch "day$1/src/lib.gleam"

cat >"day$1/src/day$1.gleam" <<EOF
import gleam/io
import gleam/string
import gleam/erlang
import lib

pub fn read_all_stdin() -> List(String) {
	case erlang.get_line("") {
		Ok(line) -> [string.trim(line), ..read_all_stdin()]
		_ -> []
	}
}

pub fn part1(lines: List(String)) -> String {
	todo
}

pub fn part2(lines: List(String)) -> String {
	todo
}

pub fn main() {
	let input = read_all_stdin()
	io.println("Part 1: " <> part1(input))
	io.println("Part 2: " <> part1(input))
}
EOF

cat >"day$1/gleam.toml" <<EOF
name = "day${1}"
version = "1.0.0"

# Fill out these fields if you intend to generate HTML documentation or publish
# your project to the Hex package manager.
#
# description = ""
# licences = ["Apache-2.0"]
# repository = { type = "github", user = "username", repo = "project" }
# links = [{ title = "Website", href = "https://gleam.run" }]

[dependencies]
gleam_stdlib = "~> 0.34"
gleam_erlang = "~> 0.24"

[dev-dependencies]
gleeunit = "~> 1.0"
