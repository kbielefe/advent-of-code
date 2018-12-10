package common
import org.scalatest._

class TestCircularZipper extends UnitSpec {
  "Circular Zipper" when {
    "initialized" should {
      val list = CircularZipper[Int]()
      "be empty" in {
        list shouldBe empty
      }

      "not error out when moving right" in {
        list.moveRight shouldBe empty
      }

      "not error out when moving left" in {
        list.moveRight shouldBe empty
      }
    }

    "one element added" should {
      val list = CircularZipper[Int]().insertRight(1)
      "not be empty" in {
        list should not be empty
      }

      "allow deleting" in {
        list.delete shouldBe empty
      }

      "have a current element" in {
        list.current shouldBe 1
      }

      "have the same current element after moving right" in {
        list.moveRight.current shouldBe 1
      }

      "have the same current element after moving left" in {
        list.moveLeft.current shouldBe 1
      }
    }

    "two elements added" should {
      val list = CircularZipper[Int]().insertRight(1).insertRight(2)
      "not be empty" in {
        list should not be empty
      }

      "allow deleting" in {
        list.delete.delete shouldBe empty
      }

      "have a current element" in {
        list.current shouldBe 2
      }

      "switch current after moving right" in {
        list.moveRight.current shouldBe 1
      }

      "switch current after moving left" in {
        list.moveLeft.current shouldBe 1
      }
    }
  }
}
