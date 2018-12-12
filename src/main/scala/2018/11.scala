package advent2018
import common.{Day, Dynamic}
import scala.io.Source

class Day11(source: Source) extends Day {

  val serialNumber = source.mkString.trim.toInt

  def hundredsDigit(n: Int): Int = {
    n / 100 % 10
  }

  def powerLevel(x: Int, y: Int): Byte = {
    val rackID = x + 10
    (hundredsDigit((rackID * y + serialNumber) * rackID) - 5).toByte
  }

  def grid: List[List[Byte]] = (1 to 300).toList map {y => (1 to 300).toList.map{x => powerLevel(x, y)}}

  lazy val cumulative: Vector[Vector[Byte]] = Dynamic.cumulativeSums(grid).map{_.toVector}.toVector

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

  // TODO: Put into a common library
  // returns an iterator of rows
  def gridToString(f: (Int, Int) => Any)(minX: Int, minY: Int, width: Int, height: Int): Iterator[String] = {
    val widths = for {
      x <- (minX to (minX + width  - 1)).iterator
      y <- (minY to (minY + height - 1)).iterator
    } yield f(x, y).toString.size
    val maxWidth = widths.max
    def rightJustify(s: String): String = (" " * (maxWidth - s.size)) + s
    def row(y: Int): String = (minX to (minX + width - 1)).map{x => rightJustify(f(x, y).toString)}.mkString(" ")
    (minY to (minY + height - 1)).iterator.map{y => row(y)}
  }

  if (serialNumber == 18) {
    gridToString{case (x, y) => cumulative(y-1)(x-1)}(32, 44, 5, 5) foreach println
    gridToString{case (x, y) => powerLevel(x, y)}(32, 44, 5, 5) foreach println
  }

  override def answer1: String = answer(Iterator(3))
  override def answer2: String = ""
  //override def answer2: String = answer((1 to 300).iterator)
  // (215,2,3) is wrong
}
