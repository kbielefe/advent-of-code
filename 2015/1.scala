import scala.io.Source

val input = Source.fromFile("input1.txt").mkString.trim

val floors = input.scanLeft(0){case (floor, char) => if (char == '(') floor + 1 else floor - 1}

val answer1 = floors.last
println(answer1)

val answer2 = floors.indexWhere(_ < 0)
println(answer2)
