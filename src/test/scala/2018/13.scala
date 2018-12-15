package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay13 extends UnitSpec {
  def example = Source.fromString("""
   */->-\
   *|   |  /----\
   *| /-+--+-\  |
   *| | |  | v  |
   *\-+-/  \-+--/
   *  \------/   """.stripMargin('*').trim)

  def example2 = Source.fromString("""
   */>-<\
   *|   |
   *| /<+-\
   *| | | v
   *\>+</ |
   *  |   ^
   *  \<->/""".stripMargin('*').trim)

  "firstCrash" when {
    "given example path" should {
      "detect a crash at 7, 3" in {
        val day = new Day13(example)
        day.firstCrash(example) shouldBe (7, 3)
      }
    }
  }

  "lastManStanding" when {
    "given second example path" should {
      "detect the last man standing at 6, 4" in {
        val day = new Day13(example)
        day.lastManStanding(example2) shouldBe (6, 4)
      }
    }
  }
}
