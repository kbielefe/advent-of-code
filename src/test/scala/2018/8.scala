package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay8 extends UnitSpec {

  def example = Source.fromString("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  "parseNode" when {
    "no children and one metadata" should {
      "return a leaf Node" in {
        val day = new Day8(example)
        day.parseNode(List(0, 1, 2)) shouldBe day.Node(List.empty, List(2))
      }
    }

    "no children and two metadata" should {
      "return a leaf Node" in {
        val day = new Day8(example)
        day.parseNode(List(0, 2, 3, 4)) shouldBe day.Node(List.empty, List(3, 4))
      }
    }

    "one child and one metadata" should {
      "return a branch Node" in {
        val day = new Day8(example)
        day.parseNode(List(1, 1, 0, 1, 3, 2)) shouldBe day.Node(List(day.Node(List.empty, List(3))), List(2))
      }
    }
  }

  "answer 1" when {
    "given example" should {
      "be 138" in {
        val day = new Day8(example)
        day.answer1 shouldBe "138"
      }
    }
  }

  "answer 2" when {
    "given example" should {
      "be 66" in {
        val day = new Day8(example)
        day.answer2 shouldBe "66"
      }
    }
  }
}
