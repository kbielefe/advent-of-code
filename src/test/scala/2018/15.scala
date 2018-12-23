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

  val exampleString2 = """
    |#######
    |#.G...#
    |#...EG#
    |#.#.#G#
    |#..G#E#
    |#.....#
    |#######""".stripMargin.trim

  val exampleString3 = """
    |#######
    |#..G..#
    |#...EG#
    |#.#G#G#
    |#...#E#
    |#.....#
    |#######""".stripMargin.trim

  val exampleString4 = """
    |#######
    |#G..#E#
    |#E#E.E#
    |#G.##.#
    |#...#E#
    |#...E.#
    |#######""".stripMargin.trim

  val exampleString5 = """
    |#######
    |#E..EG#
    |#.#G.E#
    |#E.##E#
    |#G..#.#
    |#..E#.#
    |#######""".stripMargin.trim

  val exampleString6 = """
    |#######
    |#E.G#.#
    |#.#G..#
    |#G.#.G#
    |#G..#.#
    |#...E.#
    |#######""".stripMargin.trim

  val exampleString7 = """
    |#######
    |#.E...#
    |#.#..G#
    |#.###.#
    |#E#G#G#
    |#...#G#
    |#######""".stripMargin.trim

  val exampleString8 = """
    |#########
    |#G......#
    |#.E.#...#
    |#..##..G#
    |#...##..#
    |#...#...#
    |#.G...G.#
    |#.....G.#
    |#########""".stripMargin.trim

  val exampleString9 = """
    |#######
    |#..G..#
    |#...G.#
    |#.#G#G#
    |#...#E#
    |#.....#
    |#######""".stripMargin.trim

  val exampleString10 = """
    |#######
    |#GE.#E#
    |#E#..E#
    |#G.##.#
    |#...#E#
    |#..E..#
    |#######""".stripMargin.trim

 def toGrid(string: String): Grid[Cell] = Grid(0, 0, '.', string, _ => None, charToCell _)

 val exampleGrid1 = toGrid(exampleString1)
 val exampleGrid2 = toGrid(exampleString2)
 val exampleGrid3 = toGrid(exampleString3)
 val exampleGrid4 = toGrid(exampleString4)
 val exampleGrid5 = toGrid(exampleString5)
 val exampleGrid6 = toGrid(exampleString6)
 val exampleGrid7 = toGrid(exampleString7)
 val exampleGrid8 = toGrid(exampleString8)
 val exampleGrid9 = toGrid(exampleString9)
 val exampleGrid10 = toGrid(exampleString10)

  "closestTargetSquares" when {
    "given examples" should {
      "give the best path to the targets" in {
        val goblins = List((4, 1), (5, 3), (2, 3))
        val openSquares = adjacentOpenSquares(exampleGrid1, goblins)
        closestTargetSquares(1, 1, exampleGrid1, openSquares.toSet) shouldBe Some((List((3,1), (2,1), (1,1)), 3, 1))
      }
    }
    "given an unreachable target" should {
      "return an empty list" in {
        closestTargetSquares(1, 1, exampleGrid1, Set((5, 1))) shouldBe None
      }
    }
  }

  "battleOutcome" when {
    "given example" should {
      "return example result for example 2" in {
        battleOutcome(exampleGrid2) shouldBe 27730
      }

      "return example result for example 4" in {
        battleOutcome(exampleGrid4) shouldBe 36334
      }

      "return example result for example 5" in {
        battleOutcome(exampleGrid5) shouldBe 39514
      }

      "return example result for example 6" in {
        battleOutcome(exampleGrid6) shouldBe 27755
      }

      "return example result for example 7" in {
        battleOutcome(exampleGrid7) shouldBe 28944
      }

      "return example result for example 8" in {
        battleOutcome(exampleGrid8) shouldBe 18740
      }
    }
  }

  "broken example" when {
    "possibly moving" should {
      "move to the right" in {
        val (grid, x, y) = possiblyMove(exampleGrid3, exampleGrid3.getCell(3, 1).get.asInstanceOf[Creature], 3, 1)
        x shouldBe 4
        y shouldBe 1
      }
    }

    "getting closest target squares" should {
      "not return the path to the far elf" in {
        closestTargetSquares(3, 1, exampleGrid3, Set((3,2), (4,1), (5,5))) shouldBe  Some((List((4,1), (3,1)), 4, 1))
      }
    }
  }

  "another broken example" when {
    "possibly moving" should {
      "move to the left" in {
        val (grid, x, y) = possiblyMove(exampleGrid9, exampleGrid9.getCell(3, 1).get.asInstanceOf[Creature], 3, 1)
        x shouldBe 2
        y shouldBe 1
      }
    }

    "getting closest target squares" should {
      "return the path to the far elf" in {
        closestTargetSquares(3, 1, exampleGrid9, Set((5,5))) shouldBe Some((List((5,5), (4,5), (3,5), (3,4), (2,4), (1,4), (1,3), (1,2), (1,1), (2,1), (3,1)), 5, 5))
      }
    }
  }

  "a third broken example" when {
    "possibly moving" should {
      "move up" in {
        val (grid, x, y) = possiblyMove(exampleGrid10, exampleGrid10.getCell(3, 5).get.asInstanceOf[Creature], 3, 5)
        x shouldBe 3
        y shouldBe 4
      }
    }

    "getting closest target squares" should {
      "return the reading order path" in {
        closestTargetSquares(3, 5, exampleGrid10, Set((2, 3), (1, 4))) shouldBe Some((List((2,3), (2,4), (3,4), (3,5)), 2, 3))
      }
    }
  }
}
