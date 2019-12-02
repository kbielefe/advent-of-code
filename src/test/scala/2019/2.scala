package advent2019
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay2 extends UnitSpec {
  def toSource(vector: Vector[Int]): Source = {
    Source.fromString(vector.mkString(","))
  }

  "run" when {
    "given 1,0,0,0,99" should {
      "be 2,0,0,0,99" in {
        val input = Vector(1, 0, 0, 0, 99)
        val day = new Day2(toSource(input))
        day.run(input, 0) shouldBe Vector(2, 0, 0, 0, 99)
      }
    }
  }
}
