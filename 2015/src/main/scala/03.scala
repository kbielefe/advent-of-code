package advent2015
import common.Day
import scala.io.Source

class Day3(source: Source) extends Day {
  val input = source.mkString.trim

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

  val pathSize: Int = paths(input).toSet.size
  override def answer1 = pathSize.toString

  def dropEveryOther(input: String) = input.zipWithIndex.filter{_._2 % 2 == 0}.map(_._1)

  val santaPaths = paths(dropEveryOther(input)).toSet

  val roboPaths = paths(dropEveryOther(input drop 1)).toSet

  override def answer2 = (santaPaths ++ roboPaths).size.toString
}
