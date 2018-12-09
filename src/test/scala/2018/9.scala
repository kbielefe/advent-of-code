package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

class TestDay9 extends UnitSpec {

  def example(players: Int, lastMarblePoints: Int): Source = Source.fromString(s"$players players; last marble is worth $lastMarblePoints points")

  "answer 1" when {
    "given 9 players and last marble worth 25 points" should {
      "be 32" in {
        val day = new Day9(example(9, 25))
        day.answer1 shouldBe "32"
      }
    }

    "given 10 players and last marble worth 1618 points" should {
      "be 8317" in {
        val day = new Day9(example(10, 1618))
        day.answer1 shouldBe "8317"
      }
    }

    "given 13 players and last marble worth 7999 points" should {
      "be 146373" in {
        val day = new Day9(example(13, 7999))
        day.answer1 shouldBe "146373"
      }
    }

    "given 17 players and last marble worth 1104 points" should {
      "be 2764" in {
        val day = new Day9(example(17, 1104))
        day.answer1 shouldBe "2764"
      }
    }

    "given 21 players and last marble worth 6111 points" should {
      "be 54718" in {
        val day = new Day9(example(21, 6111))
        day.answer1 shouldBe "54718"
      }
    }

    "given 30 players and last marble worth 5807 points" should {
      "be 37305" in {
        val day = new Day9(example(30, 5807))
        day.answer1 shouldBe "37305"
      }
    }
  }

  "answer 2" when {
    "given example" should {
      "be 22563" in {
        val day = new Day9(example(9, 25))
        day.answer2 shouldBe "22563"
      }
    }
  }
}
