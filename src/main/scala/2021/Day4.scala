package advent2021
import puzzleparse.{*, given}

object Day4:

  given Read[Board] with
    def read(input: String): Board =
      val grid = summon[Read[Grid[Int]]].read(input)
      Board(Set.empty, grid.map(_.swap), 0)

  type Input = Header[List[Int], MultiLine[Board]]

  def part1(input: Input): Int = answer(input.header, input.body, true)
  def part2(input: Input): Int = answer(input.header, input.body, false)

  private def answer(calledNumbers: List[Int], boards: List[Board], first: Boolean): Int =
    val winningBoards = boards.map(_.playUntilWins(calledNumbers))
    val chosenBoard = if first then winningBoards.minBy(_._2) else winningBoards.maxBy(_._2)
    chosenBoard._1.score

  case class Board(marked: Set[Pos], unmarked: Map[Int, Pos], lastCalled: Int):
    def mark(num: Int): Board =
      if unmarked.contains(num) then
        Board(marked + unmarked(num), unmarked - num, num)
      else
        this

    def score: Int = unmarked.keys.sum * lastCalled

    def wins: Boolean =
      val rows = marked.groupBy(_.row).exists(_._2.size == 5)
      val cols = marked.groupBy(_.col).exists(_._2.size == 5)
      rows || cols

    def playUntilWins(calledNumbers: List[Int]): (Board, Int) =
      calledNumbers
        .scanLeft(this)(_ mark _)
        .zipWithIndex
        .dropWhile(!_._1.wins)
        .head
  end Board
end Day4
