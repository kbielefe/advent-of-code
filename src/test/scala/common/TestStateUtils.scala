package common
import org.scalatest._
import scala.io.Source

class TestStateUtils extends UnitSpec {
  "repeatN" when {
    "repeating 0 times" should {
      "not alter the state" in {
        val repetitions = 0
        val state = StateUtils.repeatN[Int, Int](repetitions){s => (s + 1, s + 2)}
        state.runS(1234).value shouldBe 1234
      }

      "return an empty list" in {
        val repetitions = 0
        val state = StateUtils.repeatN[Int, Int](repetitions){s => (s + 1, s + 2)}
        state.runA(1234).value shouldBe List.empty[Int]
      }
    }

    "repeating 1 time" should {
      "alter the state once" in {
        val repetitions = 1
        val state = StateUtils.repeatN[Int, Int](repetitions){s => (s + 1, s + 2)}
        state.runS(1234).value shouldBe 1235
      }

      "return a one-element list" in {
        val repetitions = 1
        val state = StateUtils.repeatN[Int, Int](repetitions){s => (s + 1, s + 2)}
        state.runA(1234).value shouldBe List(1236)
      }
    }

    "repeating twice" should {
      "alter the state twice" in {
        val repetitions = 2
        val state = StateUtils.repeatN[Int, Int](repetitions){s => (s + 1, s + 2)}
        state.runS(1234).value shouldBe 1236
      }

      "return a two-element list" in {
        val repetitions = 2
        val state = StateUtils.repeatN[Int, Int](repetitions){s => (s + 1, s + 2)}
        state.runA(1234).value shouldBe List(1237, 1236)
      }
    }
  }
}
