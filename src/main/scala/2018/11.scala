package advent2018
import common.Day
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

  def gridTotalPower(width: Int, height: Int)(left: Int, top: Int): Int = {
    val powerLevels = for {
      x <- left until (left + width)
      y <- top  until (top  + height)
    } yield powerLevel(x, y)
    powerLevels.sum
  }

  def allGridPowers(width: Int, height: Int): Map[(Int, Int), Int] = {
    val powerLevels = for {
      x <- 1 until (width - 2)
      y <- 1 until (height - 2)
    } yield ((x, y) -> gridTotalPower(3, 3)(x, y))
    powerLevels.toMap
  }

  override def answer1: String = allGridPowers(300, 300).maxBy{_._2}._1.toString
  override def answer2: String = ""
}
