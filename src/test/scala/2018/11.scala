package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay11 extends UnitSpec {

  def serialNumber(n: Int) = Source.fromString(n.toString)

  "powerLevel" when {
    "given 3, 5 serial number 8" should {
      "be 4" in {
        val day = new Day11(serialNumber(8))
        day.powerLevel(3, 5) shouldBe 4
      }
    }

    "given 122, 79 serial number 57" should {
      "be -5" in {
        val day = new Day11(serialNumber(57))
        day.powerLevel(122, 79) shouldBe -5 
      }
    }

    "given 217, 196 serial number 39" should {
      "be 0" in {
        val day = new Day11(serialNumber(39))
        day.powerLevel(217, 196) shouldBe 0
      }
    }

    "given 101, 153 serial number 71" should {
      "be 4" in {
        val day = new Day11(serialNumber(71))
        day.powerLevel(101, 153) shouldBe 4
      }
    }

    "given 33, 45 serial number 18" should {
      "be 4" in {
        val day = new Day11(serialNumber(18))
        day.powerLevel(33, 45) shouldBe 4
      }
    }
  }

  "gridTotalPower" when {
    "calculating a 3x3 at 33, 45 with serial number 18" should {
      "be 29" in {
        val day = new Day11(serialNumber(18))
        day.gridTotalPower(3, 3)(33, 45) shouldBe 29
      }
    }

    "calculating a 3x3 at 21, 61 with serial number 42" should {
      "be 30" in {
        val day = new Day11(serialNumber(42))
        day.gridTotalPower(3, 3)(21, 61) shouldBe 30
      }
    }
  }

  "answer 1" when {
    "given example" should {
      "be ???" in {
        val day = new Day11(serialNumber(0))
        day.answer1
        assert(true)
      }
    }
  }

  "answer 2" when {
    "given example" should {
      "be ???" in {
        val day = new Day11(serialNumber(0))
        day.answer2
        assert(true)
      }
    }
  }
}
