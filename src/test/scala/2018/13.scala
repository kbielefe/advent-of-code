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

  "parsePath" when {
    "given example" should {
      "have a > at 2, 0" in {
        val day = new Day13(example)
        day.parsePath(example)((2, 0)) shouldBe '>'
      }

      "have a | at 4, 1" in {
        val day = new Day13(example)
        day.parsePath(example)((4, 1)) shouldBe '|'
      }
    }
  }

  "getCarts" when {
    "given example path" should {
      "have a right-facing cart at 2, 0" in {
        val day = new Day13(example)
        val carts = day.getCarts(day.parsePath(example))
        carts((2, 0, 0)) shouldBe day.Cart(2, 0, 0, '>', 'R')
      }

      "have a down-facing cart at 9, 3" in {
        val day = new Day13(example)
        val carts = day.getCarts(day.parsePath(example))
        carts((9, 3, 1)) shouldBe day.Cart(9, 3, 1, 'v', 'R')
      }
    }
  }

  "removeCartsFromPath" when {
    "given example path" should {
      "remove the cart at 2, 0" in {
        val day = new Day13(example)
        val path = day.removeCartsFromPath(day.parsePath(example))
        path((2, 0)) shouldBe '-'
      }

      "remove the cart at 9, 3" in {
        val day = new Day13(example)
        val path = day.removeCartsFromPath(day.parsePath(example))
        path((9, 3)) shouldBe '|'
      }
    }
  }

  "firstCrash" when {
    "given example path" should {
      "detect a crash at 7,3" in {
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
