package puzzleparse

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class TestRead extends AnyFreeSpec with Matchers with EitherValues:
  "Read" - {
    "of an Int" - {
      val reader = summon[Read[Int]]

      "should read an Int by itself" in {
        reader.read("12").right.value shouldBe 12
      }

      "should extract an Int out of non-ints" in {
        reader.read("before 12 after").right.value shouldBe 12
      }
    }

    "of a String" - {
      val reader = summon[Read[String]]

      "should read the entire string" in {
        reader.read("12").right.value shouldBe "12"
      }
    }

    "of a (String, Int)" - {
      val reader = summon[Read[(String, Int)]]

      "should read a single line" in {
        reader.read("forward 3").right.value shouldBe ("forward" -> 3)
      }

      "should read subsequent lines" in {
        reader.read("forward\n3").right.value shouldBe ("forward" -> 3)
      }

      "should read multiple lines" in {
        reader.read("forward\n\n3").right.value shouldBe ("forward" -> 3)
      }
    }
    /*

    "of a Pos" - {
      "should read a single line" in {
        summon[Read[Pos]].read("1,2") shouldBe Pos(1, 2)
      }

      "should read two on a line" in {
        summon[Read[(Pos, Pos)]].read("1,2 -> 3,4") shouldBe (Pos(1, 2), Pos(3, 4))
      }
    }

    "of a List[Int]" - {
      val reader = summon[Read[List[Int]]]
      val expected = List(1,2,3,4)

      "should read a single line" in {
        reader.read("1,2,3,4") shouldBe expected
      }

      "should read subsequent lines" in {
        reader.read("1\n2\n3\n4") shouldBe expected
      }

      "should read multiple lines" in {
        reader.read("1\n\n2\n\n3\n\n4") shouldBe expected
      }
    }

    "of a List[String]" - {
      val reader = summon[Read[List[String]]]

      "should read a single line" in {
        reader.read("abc") shouldBe List("abc")
      }

      "should read subsequent lines" in {
        reader.read("abc\ndef\nghi") shouldBe List("abc", "def", "ghi")
      }

      "should read multiple lines" in {
        reader.read("abc\ndef\n\nghi") shouldBe List("abc\ndef", "ghi")
      }
    }
    */
  }
end TestRead
