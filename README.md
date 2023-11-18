# advent-of-code
For sharing my adventofcode.com solutions

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

Of these, `ReadProduct` is most likely to be useful to a puzzle solver,
as you derive it to automatically parse a case class:

```scala
case class MyCaseClass(a: Int, b: String) derives ReadProduct
```

Also adds new type operators:

 - `List[A] - "delimiter"`: splits a `String` by the delimiter, then parses it
   into a `List[A]`. This uses the Scala 3 opaque type, so to your code, this
   works exactly the same as a `List[A]`, but contains enough information to
   automatically parse it.
 - `MyCaseClass ~ """(\d+) (\w+)"""`: Uses the matched groups from the regex to
   create an instance of `MyCaseClass` from an entire `String`.
 - `List[MyCaseClass] ~ """(\d+) (\w+)"""`: Splits the `String` into
   non-overlapping matches of the regex, then creates the `List[MyCaseClass]`.

So if your input is a list of integers, one per line, you can designate that as
`List[Int] - "\n"` and it will parse it out to what looks to your code like a
regular `List[Int]`.
