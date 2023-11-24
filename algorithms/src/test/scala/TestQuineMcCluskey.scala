import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import algorithms.QuineMcCluskey
import algorithms.QuineMcCluskey.Minterm

class TestQuineMcCluskey extends AnyWordSpec with Matchers {
  "a Minterm" when {
    "zero" should {
      "have a zero oneCount" in {
        Minterm(0).oneCount shouldBe 0
      }
    }

    "15" should {
      "have a 4 oneCount" in {
        Minterm(15).oneCount shouldBe 4
      }
    }
    "10" should {
      "have a 2 oneCount" in {
        Minterm(10).oneCount shouldBe 2
      }
    }
  }

  "a QuineMcCluskey" should {
    "be callable" in {
      QuineMcCluskey("ABCD", List(0,1,2,4,6,8,9,11,13,15).map(Minterm(_))*)
    }
  }
}
