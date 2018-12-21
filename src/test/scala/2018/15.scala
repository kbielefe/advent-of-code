package advent2018
import org.scalatest._
import common.{UnitSpec, Grid}
import scala.io.Source

class TestDay15 extends UnitSpec {
  val day = new Day15(Source.fromString(""))
  import day._

  val exampleString1 = """
   |#######
   |#E..G.#
   |#...#.#
   |#.G.#G#
   |#######""".stripMargin.trim

 def toGrid(string: String): Grid[Cell] = Grid(0, 0, '.', string, _ => None, charToCell _)

 val exampleGrid1 = toGrid(exampleString1)

  "closestTargetSquares" when {
    "given examples" should {
      "give the best path to the targets" in {
        val goblins = List((4, 1), (5, 3), (2, 3))
        val openSquares = adjacentOpenSquares(exampleGrid1, goblins)
        closestTargetSquares(1, 1, exampleGrid1, openSquares.toSet) shouldBe List(List((1,1), (2,1), (3,1)), List((1,1), (1,2), (2,2)), List((1,1), (1,2), (1,3)))
      }
    }
    "given an unreachable target" should {
      "return an empty list" in {
        closestTargetSquares(1, 1, exampleGrid1, Set((5, 1))) shouldBe empty
      }
    }
  }
}
