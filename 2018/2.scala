import scala.io.Source

val input = Source.fromFile("input2.txt").getLines.toList
val counts = input map {_.groupBy(identity).map{_._2.length}.toList}
def appearsN(n: Int): Int = counts count {_ contains n}

val answer1 = appearsN(2) * appearsN(3)
println(answer1)

def differsByOne(x: List[String]): Boolean = {
  val List(first, second) = x
  val differenceCount = first zip second count {case (x, y) => x != y}
  differenceCount == 1
}

def sameCharacters(x: List[String]): String = {
  val List(first, second) = x
  val same = first zip second filter {case (x, y) => x == y}
  same.map(_._1).mkString("")
}

val answer2 = input.combinations(2).find(differsByOne).map(sameCharacters)
println(answer2)
