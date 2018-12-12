package common
import org.scalatest._
import scala.io.Source

class TestDynamic extends UnitSpec {
  "cumulativeSums" when {
    "given an empty List of Lists" should {
      "return itself" in {
        val sums = Dynamic.cumulativeSums(List.empty[List[Int]])
        sums shouldBe empty
      }
    }

    "given a List of empty Lists" should {
      "return itself" in {
        val input = List(List.empty[Int], List.empty[Int])
        val sums = Dynamic.cumulativeSums(input)
        sums shouldBe input
      }
    }

    "given a List of a single element List" should {
      "return itself" in {
        val input = List(List(1))
        val sums = Dynamic.cumulativeSums(input)
        sums shouldBe input
      }
    }

    "given a List with a single row" should {
      "return the cumulative sum of that row" in {
        val input = List(List(1, 2, 3))
        val output = List(List(1, 3, 6))
        val sums = Dynamic.cumulativeSums(input)
        sums shouldBe output
      }
    }

    "given a List with two rows" should {
      "return the cumulative sum of the rows" in {
        val input  = List(List(1, 2, 3),
                          List(4, 5, 6),
                          List(7, 8, 9))
        val output = List(List(1,   3,  6),
                          List(5,  12, 21),
                          List(12, 27, 45))
        val sums = Dynamic.cumulativeSums(input)
        sums shouldBe output
      }
    }

    "given a big input" should {
      "return the correct output" in {
        val input = List(
          List(-3, 4, 2,  2,  2),
          List(-4, 4, 3,  3,  4),
          List(-5, 3, 3,  4, -4),
          List( 4, 3, 3,  4, -3),
          List( 3, 3, 3, -5, -1)
        )
        val output = List(
          List( -3,  1,  3,  5,  7),
          List( -7,  1,  6, 11, 17),
          List(-12, -1,  7, 16, 18),
          List( -8,  6, 17, 30, 29),
          List( -5, 12, 26, 34, 32)
        )
        val sums = Dynamic.cumulativeSums(input)
        sums shouldBe output
      }
    }

    "given another big input" should {
      "return the correct output" in {
        val input = List(
          List(-2,  -4,   4,   4,   4),
          List(-4,   4,   4,   4,  -5),
          List( 4,   3,   3,   4,  -4),
          List( 1,   1,   2,   4,  -3),
          List(-1,   0,   2,  -5,  -2)
        )
        val output = List(
          List(-2, -6, -2,  2,  6),
          List(-6, -6,  2, 10,  9),
          List(-2,  1, 12, 24, 19),
          List(-1,  3, 16, 32, 24),
          List(-2,  2, 17, 28, 18)
        )
        // Total power 29
        val sums = Dynamic.cumulativeSums(input)
        sums shouldBe output
      }
    }
  }
}
