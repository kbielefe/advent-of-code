package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay14 extends UnitSpec {
  val day = new Day14(Source.fromString("0"))
  "score10After" when {
    "given 9" should {
      "be 5158916779" in {
        day.score10After(9) shouldBe "5158916779"
      }
    }

    "given 5" should {
      "be 0124515891" in {
        day.score10After(5) shouldBe "0124515891"
      }
    }

    "given 18" should {
      "be 9251071085" in {
        day.score10After(18) shouldBe "9251071085"
      }
    }

    "given 2018" should {
      "be 5941429882" in {
        day.score10After(2018) shouldBe "5941429882"
      }
    }
  }
}
