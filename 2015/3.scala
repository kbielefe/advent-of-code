import scala.io.Source

val input = Source.fromFile("input3.txt").mkString.trim

val offsets = Map(
  '^' -> ( 0,  1),
  'v' -> ( 0, -1),
  '<' -> (-1,  0),
  '>' -> ( 1,  0)
)

def paths(input: Seq[Char]) = input.scanLeft((0, 0)){case ((x, y), dir) =>
  val (xOff, yOff) = offsets(dir)
  (x + xOff, y + yOff)
}

val answer1 = paths(input).toSet.size
println(answer1)

def dropEveryOther(input: String) = input.zipWithIndex.filter{_._2 % 2 == 0}.map(_._1)

val santaPaths = paths(dropEveryOther(input)).toSet

val roboPaths = paths(dropEveryOther(input drop 1)).toSet

val answer2 = (santaPaths ++ roboPaths).size

println(answer2)
