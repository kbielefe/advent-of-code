package advent2018
import org.scalatest._
import common.{UnitSpec, Grid}
import scala.io.Source

class TestDay18 extends UnitSpec {
  val example = """
    *.#.#...|#.
    *.....#|##|
    *.|..|...#.
    *..|#.....#
    *#.#|||#|#|
    *...#.||...
    *.|....|...
    *||...#|.#|
    *|.||||..|.
    *...#.|..|.""".stripMargin('*').trim

    val result = """
      *.......##.
      *......|###
      *.|..|...#.
      *..|#||...#
      *..##||.|#|
      *...#||||..
      *||...|||..
      *|||||.||.|
      *||||||||||
      *....||..|.""".stripMargin('*').trim

    val resultAfter10 = """
      *.||##.....
      *||###.....
      *||##......
      *|##.....##
      *|##.....##
      *|##....##|
      *||##.####|
      *||#####|||
      *||||#|||||
      *||||||||||""".stripMargin('*').trim

  val day = new Day18(Source.fromString(""))
  "move" when {
    val grid = Grid(0, 0, '.', example, day.under _, day.toCell _)
    val resultGrid = Grid(0, 0, '.', result, day.under _, day.toCell _)
    val resultAfter10Grid = Grid(0, 0, '.', resultAfter10, day.under _, day.toCell _)
    "given example" should {
      "have given result" in {
        day.move(grid, 10, 10) shouldBe resultGrid
      }

      "have given result after 10" in {
        day.moves(grid, 10, 10).drop(10).headOptionL.value shouldBe Some(resultAfter10Grid)
      }
    }
  }
}
