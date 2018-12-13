package advent2018
import org.scalatest._
import common.{UnitSpec, Visualize}
import scala.io.Source

class TestDay12 extends UnitSpec {
  def example = Source.fromResource("2018/example12.txt")

  "initial state" when {
    "given example" should {
      "translate to an boolean list" in {
        val day = new Day12(example)
        day.initialState shouldBe Set(0, 3, 5, 8, 9, 16, 17, 18, 22, 23, 24)
      }

      "give an answer 1 of 325" in {
        val day = new Day12(example)
        day.answer1 shouldBe "325"
      }
    }
  }

  "growGeneration" when {
    val day = new Day12(example)
    def testGrowGeneration(in: String): String = {
      val result = day.growGeneration(day.toState(in))
      Visualize.booleanRowToString{result contains _}(result.min, result.max, '#', '.')
    }
    "given #..#.#..##......###...###" should {
      "produce #...#....#.....#..#..#..#" in {
        testGrowGeneration("#..#.#..##......###...###") shouldBe "#...#....#.....#..#..#..#"
      }
    }
  }
}
