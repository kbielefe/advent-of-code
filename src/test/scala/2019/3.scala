package advent2019
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay3 extends UnitSpec {
  "answer1" when {
    "given example" should {
      "be 6" in {
        val input = Source.fromString("R8,U5,L5,D3\nU7,R6,D4,L4")
        val day = new Day3(input)
        day.answer1 shouldBe "6"
      }
    }
  }

  "answer2" when {
    "given example" should {
      "be 40" in {
        val input = Source.fromString("R8,U5,L5,D3\nU7,R6,D4,L4")
        val day = new Day3(input)
        day.answer2 shouldBe "30"
      }
    }

    "given second example" should {
      "be 610" in {
        val input = Source.fromString("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
        val day = new Day3(input)
        day.answer2 shouldBe "610"
      }
    }
  }
}
