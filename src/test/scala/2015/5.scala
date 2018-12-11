package advent2015
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay5 extends UnitSpec {
  "niceString" when {
    val day = new Day5(Source.fromString(""))
    "given ugknbfddgicrmopn" should {
      "be nice" in {
        day.niceString("ugknbfddgicrmopn") shouldBe true
      }
    }

    "given aaa" should {
      "be nice" in {
        day.niceString("aaa") shouldBe true
      }
    }

    "given jchzalrnumimnmhp" should {
      "be naughty" in {
        day.niceString("jchzalrnumimnmhp") shouldBe false
      }
    }

    "given haegwjzuvuyypxyu" should {
      "be naughty" in {
        day.niceString("haegwjzuvuyypxyu") shouldBe false
      }
    }

    "given dvszwmarrgswjxmb" should {
      "be naughty" in {
        day.niceString("dvszwmarrgswjxmb") shouldBe false
      }
    }
  }

  "containsForbiddenStrings" when {
    val day = new Day5(Source.fromString(""))
    "given haegwjzuvuyypxyu" should {
      "be true" in {
        day.containsForbiddenStrings("haegwjzuvuyypxyu") shouldBe true
      }
    }
  }

  "niceString part 2" when {
    val day = new Day5(Source.fromString(""))

    "given qjhvhtzxzqqjkmpb" should {
      "be nice" in {
        day.niceStringPart2("qjhvhtzxzqqjkmpb") shouldBe true
      }
    }

    "given xxyxx" should {
      "be nice" in {
        day.niceStringPart2("xxyxx") shouldBe true
      }
    }

    "given uurcxstgmygtbstg" should {
      "be naughty" in {
        day.niceStringPart2("uurcxstgmygtbstg") shouldBe false
      }
    }

    "given ieodomkazucvgmuy" should {
      "be naughty" in {
        day.niceStringPart2("ieodomkazucvgmuy") shouldBe false
      }
    }
  }
}
