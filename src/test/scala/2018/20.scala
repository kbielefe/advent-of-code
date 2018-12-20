package advent2018
import org.scalatest._
import common.UnitSpec
import scala.io.Source

class TestDay20 extends UnitSpec {
  val day = new Day20(Source.fromString(""))
  import day._
  "parseRegex" when {
    "given an empty regex" should {
      "return an empty Sequence" in {
        parseRegex("^$") shouldBe Sequence(List.empty)
      }
    }

    "given a regex with a single move" should {
      "return that move" in {
        parseRegex("^N$") shouldBe Sequence(List(Move('N')))
      }
    }

    "given a regex with a simple list of moves" should {
      "return those moves" in {
        parseRegex("^NSEW$") shouldBe Sequence(List(Move('N'), Move('S'), Move('E'), Move('W')))
      }
    }

    "given a regex with a branch with one option" should {
      "return that branch as one element of the sequence" in {
        parseRegex("^N(S)$") shouldBe Sequence(List(Move('N'), Branch(List(Sequence(List(Move('S')))))))
      }
    }

    "given a regex with a branch with multiple options" should {
      "return those options" in {
        parseRegex("^(N|S)$") shouldBe Sequence(List(Branch(List(Sequence(List(Move('N'))), Sequence(List(Move('S')))))))
      }
    }

    "given the real input" should {
      "not overflow the stack" in {
        val regex = parseRegex(Source.fromResource("2018/20.txt").mkString.trim)
        println(regex)
        assert(true)
      }
    }
  }
}
