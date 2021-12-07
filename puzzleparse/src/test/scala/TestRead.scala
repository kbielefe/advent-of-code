package puzzleparse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TestRead extends AnyFreeSpec with Matchers:
  "Read" - {
    "of an Int" - {
      val reader = summon[Read[Int]]

      "should read an Int by itself" in {
        reader.read("12") shouldBe 12
      }

      "should extract an Int out of non-ints" in {
        reader.read("before 12 after") shouldBe 12
      }
    }

    "of a String" - {
      val reader = summon[Read[String]]

      "should read the entire string" in {
        reader.read("12") shouldBe "12"
      }
    }

    "of a (String, Int)" - {
      val reader = summon[Read[(String, Int)]]

      "should read a single line" in {
        reader.read("forward 3") shouldBe ("forward" -> 3)
      }

      "should read subsequent lines" in {
        reader.read("forward\n3") shouldBe ("forward" -> 3)
      }

      "should read multiple lines" in {
        reader.read("forward\n\n3") shouldBe ("forward" -> 3)
      }
    }
  }
end TestRead
