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

    "given a regex with a move after an empty branch" should {
      "include the last move" in {
        parseRegex("^N(E|)W$") shouldBe Sequence(List(Move('N'), Branch(List(Sequence(List(Move('E'))), Sequence(List()))), Move('W')))
      }
    }
    "given the real input" should {
      "not overflow the stack" in {
        val regex = parseRegex(Source.fromResource("2018/20.txt").mkString.trim)
        assert(true)
      }
    }
  }

  "Move.traverse" when {
    "traversing a single-element set of endpoints" should {
      "return those endpoints moved, and a path of length 1" in {
        val (endpoints, paths) = Move('N').traverse(Set((0, 0)), Map.empty)
        endpoints shouldBe Set((0, -1))
        paths shouldBe Map(((0, -1) -> ((0, 0), 1)))
      }
    }
  }

  "Sequence.traverse" when {
    "traversing a single-element set of endpoints" should {
      "return those endpoints moved, and a path of length 2" in {
        val (endpoints, paths) = Sequence(List(Move('N'), Move('E'))).traverse(Set((0, 0)), Map.empty)
        endpoints shouldBe Set((1, -1))
        paths shouldBe Map(((0, -1) -> ((0, 0), 1)), ((1, -1) -> ((0, -1), 2)))
      }
    }

    "traversing an empty sequence" should {
      "return everything unchanged" in {
        val (endpoints, paths) = Sequence(List()).traverse(Set((0, 0)), Map.empty)
        endpoints shouldBe Set((0, 0))
        paths shouldBe empty
      }
    }
  }

  "Branch.traverse" when {
    "traversing a single-element set of endpoints" should {
      "return that endpoint split and  moved, and paths of length 1" in {
        val (endpoints, paths) = Branch(List(Move('N'), Move('E'))).traverse(Set((0, 0)), Map.empty)
        endpoints shouldBe Set((0, -1), (1, 0))
        paths shouldBe Map(((0, -1) -> ((0, 0), 1)), ((1, 0) -> ((0, 0), 1)))
      }
    }

    "branch has an empty sequence" should {
      "return one split of length 1 and one of length 0" in {
        val (endpoints, paths) = Branch(List(Move('N'), Sequence(List()))).traverse(Set((0, 0)), Map.empty)
        endpoints shouldBe Set((0, 0), (0, -1))
        paths shouldBe Map(((0, -1) -> ((0, 0), 1)))
      }
    }
  }

  "Regex.traverse" when {
    "containing empty branches" should {
      "handle it correctly" in {
        val regex = parseRegex("^N(E|)W$")
        println(regex)
        val (endpoints, paths) = regex.traverse(Set((0, 0)), Map.empty)
        endpoints shouldBe Set((0, -1), (-1, -1))
        paths shouldBe Map(((0, -1) -> ((0, 0), 1)), ((1, -1) -> ((0, -1), 2)), ((-1, -1) -> ((0, -1), 2)))
      }
    }
  }
  "furthestRoomDistance" when {
    "given examples" should {
      "give correct results" in {
        furthestRoomDistance("^WNE$") shouldBe 3
        furthestRoomDistance("^ENWWW(NEEE|SSE(EE|N))$") shouldBe 10
        furthestRoomDistance("^N(E|)W$") shouldBe 2
        furthestRoomDistance("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") shouldBe 18
        furthestRoomDistance("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$") shouldBe 23
        furthestRoomDistance("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$") shouldBe 31
      }
    }
  }
}
