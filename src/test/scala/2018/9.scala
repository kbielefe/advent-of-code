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
      "be ???" in {
        val day = new Day9(example(9, 32))
        day.answer2 shouldBe ""
      }
    }
  }

  /*
  "moves" when {
    val day = new Day9(example(0, 0))
    "not a multiple of 23" should {
      "skip one each time" in {
        day.moves.take(10).toList shouldBe List(
          (0, 0, 0, LinkedCircular(0~>0)),
          (1, 1, 0, LinkedCircular(0~>1, 1~>0)),
          (2, 2, 0, LinkedCircular(0~>2, 2~>1, 1~>0)),
          (3, 3, 0, LinkedCircular(0~>2, 2~>1, 1~>3, 3~>0)),
          (4, 4, 0, LinkedCircular(0~>4, 4~>2, 2~>1, 1~>3, 3~>0)),
          (5, 5, 0, LinkedCircular(0~>4, 4~>2, 2~>5, 5~>1, 1~>3, 3~>0)),
          (6, 6, 0, LinkedCircular(0~>4, 4~>2, 2~>5, 5~>1, 1~>6, 6~>3, 3~>0)),
          (7, 7, 0, LinkedCircular(0~>4, 4~>2, 2~>5, 5~>1, 1~>6, 6~>3, 3~>7, 7~>0)),
          (8, 8, 0, LinkedCircular(0~>8, 8~>4, 4~>2, 2~>5, 5~>1, 1~>6, 6~>3, 3~>7, 7~>0)),
          (9, 9, 0, LinkedCircular(0~>8, 8~>4, 4~>9, 9~>2, 2~>5, 5~>1, 1~>6, 6~>3, 3~>7, 7~>0)))
      }
    }

    "a multiple of 23" should {
      "replace counterclockwise 7" in {
        day.moves.drop(22).take(3).toList shouldBe List(
          (22, 22, 0,  Graph(0~>16, 16~>8, 8~>17, 17~>4, 4~>18, 18~>9, 9~>19, 19~>2, 2~>20, 20~>10, 10~>21, 21~>5, 5~>22, 22~>11, 11~>1, 1~>12, 12~>6, 6~>13, 13~>3, 3~>14, 14~>7, 7~>15, 15~>0)),
          (19, 23, 32, Graph(0~>16, 16~>8, 8~>17, 17~>4, 4~>18, 18~>19, 19~>2, 2~>20, 20~>10, 10~>21, 21~>5, 5~>22, 22~>11, 11~>1, 1~>12, 12~>6, 6~>13, 13~>3, 3~>14, 14~>7, 7~>15, 15~>0)),
          (24, 24, 0,  Graph(0~>16, 16~>8, 8~>17, 17~>4, 4~>18, 18~>19, 19~>2, 2~>24, 24~>20, 20~>10, 10~>21, 21~>5, 5~>22, 22~>11, 11~>1, 1~>12, 12~>6, 6~>13, 13~>3, 3~>14, 14~>7, 7~>15, 15~>0)))
      }
    }
  }
  */
}
