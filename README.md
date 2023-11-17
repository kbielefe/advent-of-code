# advent-of-code
For sharing my adventofcode.com solutions

# Runner
Runs the puzzles. Run `run --help` from the sbt console of one of the project years for options.

# Algorithms
A set of useful algorithms for solving the puzzles.

# Parse
Automatically parses puzzle input according to the given type.

1: List[List[Int :| Positive] - "\n"] - "\n\n"
2: List[(Char, Char) - " "] - "\n"
3: List[String] - "\n"
4: List[((Range - "-"), (Range - "-")) - ","] - "\n"
5: (String, List[(Digit, Digit, Digit) ~ "move (\d) from (\d) to (\d)"] - "\n") - "\n\n"
6: String
7: List[(Cd ~ "$ cd (.+)") | "$ ls" | (Dir ~ "dir (.+)") | (File ~ "(\d+) (.+)")] - "\n"
8: Grid[Digit]
9: List[(Char, Int) - " "] - "\n"
10: List[(Noop ~ "noop") | (Addx ~ "addx (-?\d+)")] - "\n"
11: complex monkeys, but doable with this representation
12: Grid[Char]
13: List[(String, String) - "\n"] - "\n\n"
14: List[List[(Int, Int) - ","] - " -> "] - "\n"
15: List[Sensor ~ "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"] - "\n"

Parts of the type:
 - Base type representation that has a Read type class instance
 - Delimiter between list or product elements (-)
 - Regex with match group per product element (~)

How does iron make the IronType act like the base type?
 - opaque type IronType[A, C] <: A = A
