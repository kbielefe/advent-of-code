package advent2018
import common.{Day, Dynamic, Visualize}
import scala.io.Source

class Day11(source: Source) extends Day {

  val serialNumber = source.mkString.trim.toInt

  def hundredsDigit(n: Int): Int = {
    n / 100 % 10
  }

  def powerLevel(x: Int, y: Int): Int = {
    val rackID = x + 10
    hundredsDigit((rackID * y + serialNumber) * rackID) - 5
  }

  def grid: List[List[Int]] = (1 to 300).toList map {y => (1 to 300).toList.map{x => powerLevel(x, y)}}

  lazy val cumulative: Vector[Vector[Int]] = Dynamic.cumulativeSums(grid).map{_.toVector}.toVector

  def gridPower(x: Int, y: Int, size: Int): ((Int, Int, Int), Int) = {
    val power = cumulative(y + size - 2)(x + size - 2) -
    (if (y >= 2) cumulative(y - 2)(x + size - 2) else 0) -
    (if (x >= 2) cumulative(y + size - 2)(x - 2) else 0) +
    (if (x >= 2 && y >= 2) cumulative(y - 2)(x - 2) else 0)
    ((x, y, size), power)
  }

  def answer(sizes: Iterator[Int]): String = {
    val gridPowers = for {
      size <- sizes
      x    <- (1 to (301 - size)).iterator
      y    <- (1 to (301 - size)).iterator
    } yield gridPower(x, y, size)
    gridPowers.maxBy{_._2}._1.toString
  }

  override def answer1: String = answer(Iterator(3))
  override def answer2: String = answer((1 to 300).iterator)
}
