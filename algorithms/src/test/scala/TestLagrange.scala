import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import algorithms.Lagrange

class TestLagrange extends AnyWordSpec with Matchers {
  "Lagrange" when {
    "f(x) = x + 1 and" when {
      "computing coefficients directly from zeroes" should {
        "have coefficients of 1, 1" in {
          Lagrange.coefficients(List(-1)) shouldBe List(1, 1)
        }
      }

      "computing coefficients from two points" should {
        "have coefficients of 1, 1" in {
          val l = Lagrange(Vector((1, 2), (2, 3)))
          l.denom shouldBe 1
          l.coefficients shouldBe List(1, 1)
        }
      }

      "interpolating f(3)" should {
        "have a result of 4" in {
          val l = Lagrange(Vector((1, 2), (2, 3)))
          l(3) shouldBe 4
        }
      }
    }
  }
}
