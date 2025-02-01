package advent2017
import common.Day
import scala.io.Source

class Day6(source: Source) extends Day {
  val input = source.mkString.split("\\s+").map(_.toInt).toVector

  def redistribute(input: Vector[Int]): Vector[Int] = {
    val max = input.max
    val index = input.indexWhere(_ == max)
    val zeroed = input.updated(index, 0)
    (1 to max).foldLeft(zeroed){case (xs, offset) =>
      val rotI = (index + offset) % input.size
      xs.updated(rotI, xs(rotI) + 1)
    }
  }

  val redistributions = Stream.iterate(input)(redistribute)
  val cumulative = redistributions.scanLeft(Set.empty[Vector[Int]])(_ + _)
  val zipped = redistributions zip cumulative
  val results = zipped takeWhile {case (x, seen) => !seen.contains(x)}
  override def answer1 = results.size.toString

  val looped = redistribute(results.last._1)
  val beforeLoop = redistributions takeWhile {_ != looped}
  override def answer2 = (answer1.toInt - beforeLoop.size).toString
}
