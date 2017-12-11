import scala.io.Source
import Math.abs

val input = Source.fromFile("input11.txt").mkString.split(",").map(_.trim)

val offsets = Map(
  "n"  -> ( 0,  1, -1),
  "s"  -> ( 0, -1,  1),
  "ne" -> ( 1,  0, -1),
  "sw" -> (-1,  0,  1),
  "nw" -> (-1,  1,  0),
  "se" -> ( 1, -1,  0)
)

val coords = input.scanLeft((0, 0, 0)){ case ((x, y, z), dir) =>
  val (xOff, yOff, zOff) = offsets(dir)
  (x + xOff, y + yOff, z + zOff)
}

def distance(coords: (Int, Int, Int)): Int = List(abs(coords._1), abs(coords._2), abs(coords._3)).max

val answer1 = distance(coords.last)
println(answer1)

val answer2 = coords.map(distance).max
println(answer2)
