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

  def gridTotalPower(size: Int)(left: Int, top: Int): Int = {
    val powerLevels = for {
      x <- left until (left + size)
      y <- top  until (top  + size)
    } yield powerLevel(x, y)
    powerLevels.sum
  }

  def allGridPowers(size: Int): Map[(Int, Int, Int), Int] = {
    val powerLevels = for {
      x <- 1 until (301 - size)
      y <- 1 until (301 - size)
    } yield ((x, y, size) -> gridTotalPower(size)(x, y))
    powerLevels.toMap
  }

  override def answer1: String = allGridPowers(3).maxBy{_._2}._1.toString
  override def answer2: String = (1 to 300).map(allGridPowers).foldLeft(Map.empty[(Int, Int, Int), Int]){_ ++ _}.maxBy{_._2}._1.toString
}
