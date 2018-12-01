import scala.io.Source

val input = Source.fromFile("input1.txt").getLines.map(_.toInt).toList
def repeated = Iterator.continually(input).flatten.scanLeft(0){_ + _}
val cumulative = repeated.scanLeft(Set.empty[Int]){case (seen, next) => seen + next}

val answer1 = input.sum
val answer2 = (cumulative zip repeated).dropWhile{case (seen, next) => !seen.contains(next)}.next._2

println(answer1)
println(answer2)
