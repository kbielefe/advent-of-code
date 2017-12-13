import scala.io.Source

val input = Source.fromFile("input2.txt").getLines.map(_.split("\\s+") map {_.toInt}).toList

val answer1 = input.map{x => x.max - x.min}.sum
println(answer1)
val answer2 = input.map(_.combinations(2)
  .map{_.sorted}
  .filter{case Array(x,y) => y % x == 0}
  .map{case Array(x,y) => y / x}
  .next).sum
println(answer2)
