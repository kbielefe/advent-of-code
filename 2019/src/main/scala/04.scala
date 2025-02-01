package advent2019
import common.Day
import common.Numeric._
import scala.io.Source

class Day4(source: Source) extends Day {
  val validRange = 138307 to 654504

  def alwaysIncreasing(passcode: Int): Boolean = {
    passcode.digits.sliding(2).forall{case Seq(a, b) => b >= a}
  }

  def hasDouble(passcode: Int): Boolean = {
    passcode.digits.sliding(2).exists{case Seq(a, b) => a == b}
  }

  def hasExactDouble(passcode: Int): Boolean = {
    passcode.digits.sliding(2).exists{case Seq(a, b) => a == b && passcode.digits.count(_ == a) == 2}
  }

  override def answer1 = validRange.filter(alwaysIncreasing).filter(hasDouble).length.toString

  override def answer2 = validRange.filter(alwaysIncreasing).filter(hasExactDouble).length.toString
}
