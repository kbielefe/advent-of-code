package common
import org.scalatest._
import scala.io.Source
import cats.data.State

class TestStateUtils extends UnitSpec {
  "repeatN" when {
    val repeatedState = State((s: Int) => (s + 1, s + 2))
    "repeating 0 times" should {
      "not alter the state" in {
        val state = StateUtils.repeatN[Int, Int](0)(repeatedState)
        state.runS(1234).value shouldBe 1234
      }

      "return an empty list" in {
        val state = StateUtils.repeatN[Int, Int](0)(repeatedState)
        state.runA(1234).value shouldBe List.empty[Int]
      }
    }

    "repeating 1 time" should {
      "alter the state once" in {
        val state = StateUtils.repeatN[Int, Int](1)(repeatedState)
        state.runS(1234).value shouldBe 1235
      }

      "return a one-element list" in {
        val state = StateUtils.repeatN[Int, Int](1)(repeatedState)
        state.runA(1234).value shouldBe List(1236)
      }
    }

    "repeating twice" should {
      "alter the state twice" in {
        val state = StateUtils.repeatN[Int, Int](2)(repeatedState)
        state.runS(1234).value shouldBe 1236
      }

      "return a two-element list" in {
        val state = StateUtils.repeatN[Int, Int](2)(repeatedState)
        state.runA(1234).value shouldBe List(1236, 1237)
      }
    }

    "repeating 1 million times" should {
      "not overflow the stack" in {
        val state = StateUtils.repeatN[Int, Int](1000000)(repeatedState)
        state.runS(1234).value shouldBe 1001234
      }
    }
  }
}
