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

  "repeatWhile" when {
    val repeatedState = State((s: Int) => (s + 1, s + 2))
    "immediately false" should {
      "not change the state" in {
        val state = StateUtils.repeatWhile[Int, Int]{_ => false}(repeatedState)
        state.runS(1234).value shouldBe 1234
      }

      "return an empty list" in {
        val state = StateUtils.repeatWhile[Int, Int]{_ => false}(repeatedState)
        state.runA(1234).value shouldBe List.empty[Int]
      }
    }

    "true for one iteration" should {
      "change the state once" in {
        val state = StateUtils.repeatWhile[Int, Int]{_ <= 1234}(repeatedState)
        state.runS(1234).value shouldBe 1235
      }

      "return a single-element list" in {
        val state = StateUtils.repeatWhile[Int, Int]{_ <= 1234}(repeatedState)
        state.runA(1234).value shouldBe List(1236)
      }
    }

    "true for two iterations" should {
      "change the state twice" in {
        val state = StateUtils.repeatWhile[Int, Int]{_ <= 1235}(repeatedState)
        state.runS(1234).value shouldBe 1236
      }

      "return a two-element list" in {
        val state = StateUtils.repeatWhile[Int, Int]{_ <= 1235}(repeatedState)
        state.runA(1234).value shouldBe List(1236, 1237)
      }
    }
  }

  "foreach" when {
    val sumState = State[(Int, Int), Int](s => ((s._1 + s._2, s._1 + s._2), s._1 + s._2))
    "given an empty list" should {
      "not modify the state" in {
        val state = StateUtils.foreach(sumState)
        state.runS((1234, List.empty[Int])).value shouldBe (1234, ())
      }
    }
  }
}
