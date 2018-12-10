package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay10 extends UnitSpec {

  def example = Source.fromResource("2018/example10.txt")

  "answer 1" when {
    "given example" should {
      "be ???" in {
        val day = new Day10(example)
        day.answer1
        assert(true)
      }
    }
  }

  "answer 2" when {
    "given example" should {
      "be ???" in {
        val day = new Day10(example)
        day.answer2
        assert(true)
      }
    }
  }
}
