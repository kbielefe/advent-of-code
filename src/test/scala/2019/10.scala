package advent2019
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay10 extends UnitSpec {
  "answer1" when {
    "given example" should {
      "be 8" in {
        val input = Source.fromString(".#..#\n.....\n#####\n....#\n...##")
        val day = new Day10(input)
        day.answer1 shouldBe "8"
      }
    }

    "given second example" should {
      "be 33" in {
        val input = Source.fromString(
       """|......#.#.
          |#..#.#....
          |..#######.
          |.#.#.###..
          |.#..#.....
          |..#....#.#
          |#..#....#.
          |.##.#..###
          |##...#..#.
          |.#....####""".stripMargin)
        val day = new Day10(input)
        day.answer1 shouldBe "33"
      }
    }

    "given specific blocking example" should {
      "block specified values" in {
        val input = Source.fromString(
          """|#.........
             |...#......
             |...#..#...
             |.####....#
             |..#.#.#...
             |.....#....
             |..###.#.##
             |.......#..
             |....#...#.
             |...#..#..#""".stripMargin)

        val day = new Day10(input)
        val blockers = day.blockers(0, 0) _
        blockers(3, 1) shouldBe Set((6, 2), (9, 3))
        blockers(3, 2) shouldBe Set((6, 4), (9, 6))
        blockers(3, 3) shouldBe Set((4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (9, 9))
        blockers(2, 3) shouldBe Set((4, 6), (6, 9))
        blockers(2, 4) shouldBe Set((3, 6), (4, 8))
        blockers(4, 3) shouldBe Set((8, 6))
      }
    }
  }
}
