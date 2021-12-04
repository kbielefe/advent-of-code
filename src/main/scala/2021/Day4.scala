package advent2021
import puzzleparse.{*, given}

object Day4:

  given Read[Board] with
    def read(input: String): Board =
      val grid = summon[Read[Grid[Int]]].read(input)
      Board(Set.empty, grid.map(_.swap), 0)

  type Input = Header[List[Int], MultiLine[Board]]
  type BI = (Board, Int)

  def part1(input: Input): Int = answer(input, _.minBy)
  def part2(input: Input): Int = answer(input, _.maxBy)

  private def answer(input: Input, f: Seq[BI] => (BI => Int) => BI): Int =
    f(input.body.map(_.playUntilWins(input.header)))(_._2)._1.score

  case class Board(marked: Set[Pos], unmarked: Map[Int, Pos], lastCalled: Int):
    def mark(num: Int): Board =
      if unmarked.contains(num) then
        Board(marked + unmarked(num), unmarked - num, num)
      else
        this

    def score: Int = unmarked.keys.sum * lastCalled

    def wins: Boolean =
      val rows = marked.groupBy(_._1).exists(_._2.size == 5)
      val cols = marked.groupBy(_._2).exists(_._2.size == 5)
      rows || cols

    def playUntilWins(calledNumbers: List[Int]): (Board, Int) =
      calledNumbers
        .scanLeft(this)(_ mark _)
        .zipWithIndex
        .dropWhile(!_._1.wins)
        .head
  end Board
end Day4
