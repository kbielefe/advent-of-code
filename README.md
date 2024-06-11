# advent-of-code
For sharing my adventofcode.com solutions

## Visualizations

Visualizations are found on my [GitHub pages site](https://kbielefe.github.io/advent-of-code/).

## Runner
Runs the puzzles. Run `run --help` from the sbt console of one of the project years for options.

You can manually manipulate the database with `sqlite3 advent.db`.

## Algorithms
A set of useful algorithms for solving the puzzles.

## Parse
Automatically parses puzzle input according to the given type.

Adds four type classes that ease working with puzzle input:

 - `Read[A]`: Converts a `String` to an `A`.
 - `ReadSeq[C[_]]`: Converts a `String` to a container `C[A]`.
 - `ReadProduct[T]`: Converts a `String` to a tuple or case class `T`.
 - `Show[A]`: Converts an `A` into a `String`.

There are three useful overloads of `Read.apply` for different situations:

 - `given Read[MyCaseClass] = Read(regex)` Uses the match groups of the regex to populate the fields of the case class.
 - `given Read[MyCaseClass] = Read(delimiter)` Splits the input by the delimiter and assigns to each field of the case class.
 - `given Read[List[A]] = Read(delimiter)` Splits the input by the delimiter to populate each element of the List.

 The first two work with any product, so case classes and tuples. The last one
 works with any collection with a `ReadSeq` instance, which is currently
 `Vector`, `List`, and `Set`.

 `Read` also has a `.map` method, which is useful for transforming the data in
 a more complex way than delimiters or regular expressions, such as for
 building a `Graph`. It can help avoid repetition in the puzzle parts.

## Automation Guidelines

This repo follows the [automation guidelines](https://www.reddit.com/r/adventofcode/wiki/faqs/automation)
on the [/r/adventofcode](https://www.reddit.com/r/adventofcode) community wiki.

Specifically:

 - Does not require throttling because does not make outbound calls except when a human requests to run a new puzzle.
 - Once inputs are downloaded, they are cached locally ([setInput](https://github.com/kbielefe/advent-of-code/blob/9e23cd6d6cbd5b6739268de42393e7d07d194a04/runner/src/main/scala/Database.scala#L38)).
 - Previous guesses are remembered, to prevent submitting known-incorrect answers.
 - The User-Agent header is set to me ([request](https://github.com/kbielefe/advent-of-code/blob/9e23cd6d6cbd5b6739268de42393e7d07d194a04/runner/src/main/scala/Http.scala#L21)).
 - Includes this notice in the README.
 - Only stores inputs and examples in a local database file that is not committed to source control.
