package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay1 extends UnitSpec {
  def toSource(list: List[Int]): Source = {
    Source.fromString(list.mkString("\n"))
  }

  "answer 1" when {
    "given +1, -2, +3, +1" should {
      "be 3" in {
        val day = new Day1(toSource(List(1, -2, 3, 1)))
        day.answer1 shouldBe "3"
      }
    }
  }

  "answer 2" when {
    "given +1, -2, +3, +1" should {
      "be 2" in {
        val day = new Day1(toSource(List(1, -2, 3, 1)))
        day.answer2 shouldBe "2"
      }
    }
  }
}
