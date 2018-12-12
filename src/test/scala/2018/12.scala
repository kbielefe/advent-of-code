package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay12 extends UnitSpec {

  def example = Source.fromResource("2018/example12.txt")

  "initial state" when {
    "given example" should {
      "translate to an boolean list" in {
        val day = new Day12(example)
        day.initialState shouldBe List(true, false, false, true, false, true, false, false, true, true, false, false, false, false, false, false, true, true, true, false, false, false, true, true, true)
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
      val result = day.growGeneration(day.toBooleanList(in).zipWithIndex)
      val string = result.map{x => if (x._1) '#' else '.'}.mkString
      println(string)
      string
    }
    "given #..#.#..##......###...###" should {
      "produce #...#....#.....#..#..#..#" in {
        testGrowGeneration("#..#.#..##......###...###") shouldBe "#...#....#.....#..#..#..#"
      }
    }
  }
}
