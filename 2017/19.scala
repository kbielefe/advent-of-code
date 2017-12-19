import scala.io.Source

val input = Source.fromFile("input19.txt").getLines.zipWithIndex.flatMap{case (line, row) =>
  line.zipWithIndex.map{case (char, col) => (row, col) -> char}.filterNot{_._2 == ' '}
}.toMap

input foreach println
