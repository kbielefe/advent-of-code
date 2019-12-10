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
  }
}
