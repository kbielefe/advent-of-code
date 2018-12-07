package advent2017
import common.Day
import scala.io.Source

class Day15(source: Source) extends Day {
  val Array(aStart, bStart) = source.mkString.trim.split(",").map{_.toInt}
  def generator(start: Int, factor: Int): Iterator[Long] =
    Iterator.iterate(start.toLong)(_ * factor % 2147483647) drop 1 map (_ & 0xffff)

  def countEqual(a: Iterator[Long], b: Iterator[Long], take: Int): Int =
    (a zip b).take(take).count{case (x, y) => x == y}

  def a = generator(aStart, 16807)
  def b = generator(bStart, 48271)

  override def answer1 = countEqual(a, b, 40000000).toString
  override def answer2 = countEqual(a filter (_ % 4 == 0), b filter (_ % 8 == 0), 5000000).toString
}
