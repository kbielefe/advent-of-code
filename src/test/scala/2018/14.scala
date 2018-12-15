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

  "scoreboard.patternPosition" when {
    "given 123456789 and searching for 6789" should {
      "be 5" in {
        val scoreboard = day.Scoreboard(1, 2, 3, 4, 5, 6, 7, 8, 9)
        scoreboard.patternPosition("6789") shouldBe Some(5)
      }
    }

    "given 123456789 and searching for 678" should {
      "be 5" in {
        val scoreboard = day.Scoreboard(1, 2, 3, 4, 5, 6, 7, 8, 9)
        scoreboard.patternPosition("678") shouldBe Some(5)
      }
    }

    "given 3, 7, 1, 0, 1, 0, 1, 2, 4, 5 and searching for 01245" should {
      "be 5" in {
        val scoreboard = day.Scoreboard(3, 7, 1, 0, 1, 0, 1, 2, 4, 5)
        scoreboard.patternPosition("01245") shouldBe Some(5)
      }
    }
  }

  "patternPosition" when {
    "given 51589" should {
      "be 9" in {
        day.patternPosition("51589") shouldBe 9
      }
    }

    "given 01245" should {
      "be 5" in {
        day.patternPosition("01245") shouldBe 5
      }
    }

    "given 92510" should {
      "be 18" in {
        day.patternPosition("92510") shouldBe 18 
      }
    }

    "given 59414" should {
      "be 2018" in {
        day.patternPosition("59414") shouldBe 2018
      }
    }
  }
}
