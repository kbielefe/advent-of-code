# advent-of-code
For sharing my adventofcode.com solutions

To set up:

 - Place your session cookie in a file called `input/session`.
 - Create a new Day file.
 - Add the new day to the `advent` function in `Runner.scala`.
 - Start the sbt console.

Available sbt commands:

 - `run <year> <day> <part>` Runs the specified puzzle against your official input.
 - `run <year> <day> <part> example 1` Runs the puzzle against an example input in file `input/<year>/<day>_example_1.txt`.
 - `run <year> <day> <part> example 1 <answer>` Remembers the correct answer for the specified example.
 - `run <year> <day> <part> correct` Remembers that the calculated answer is correct. Useful when refactoring.
 - `run <year> <day> <part> incorrect` Remembers that the calculated answer is incorrect.
 - `run <year> <day> <part> high` Remembers that the calculated answer is too high.
 - `run <year> <day> <part> low` Remembers that the calculated answer is too low.
