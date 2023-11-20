import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import algorithms.detectCycle

class TestCycleDetection extends AnyWordSpec with Matchers {
  "detectCycle" should {
    "not detect a cycle in an empty iterator" in {
      detectCycle(Iterator.empty[Int]) shouldBe None
    }

    "detect a cycle that starts at the beginning" in {
      detectCycle(Iterator.continually(1 to 5).flatten) should contain (0, 5)
    }

    "detect a cycle that doesn't start at the beginning" in {
      detectCycle(Iterator.from(1 to 4) ++ Iterator.continually(5 to 9).flatten) should contain (4, 5)
    }

    "detect a cycle that repeats some numbers out of order" in {
      detectCycle(Iterator.from(1 to 4) ++ Iterator.continually((5 to 9) ++ (9 to 6 by -1)).flatten) should contain (4, 9)
    }
  }
}
