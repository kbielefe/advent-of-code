import scala.io.Source

val input = Source.fromFile("input5.txt").mkString("").trim()

def reacts(x: Char, y: Char): Boolean = {
  (x.toUpper == y.toUpper) && (x.isUpper != y.isUpper)
}

def react(input: String): Int = {
  val result = input.foldLeft(List.empty[Char]){case (prefix, x) =>
    if (prefix.isEmpty || !reacts(prefix.head, x))
      x :: prefix
    else
      prefix.tail
  }
  result.size
}

def filterAndReact(input: String, remove: Char): Int = {
  val filtered = input filter {x => x != remove && x != remove.toUpper}
  react(filtered)
}

val answer1 = react(input)
val answer2 = ('a' to 'z').map{x => filterAndReact(input, x)}.min

println(answer1)
println(answer2)
