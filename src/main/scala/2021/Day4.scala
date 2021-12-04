package advent2021
import puzzleparse.{*, given}

object Day4:

  given Read[Board] with
    def read(input: String): Board =
      val grid = summon[Read[Grid[Int]]].read(input)
      val unmarked = grid.map((pos, num) => (num -> pos))
      Board(Map.empty, unmarked, 0)

  def part1(input: Header[List[Int], MultiLine[Board]]): Int =
    input.body.map(_.playUntilWins(input.header)).minBy(_._2)._1.score

  def part2(input: Header[List[Int], MultiLine[Board]]): Int =
    input.body.map(_.playUntilWins(input.header)).maxBy(_._2)._1.score

  case class Board(marked: Map[Int, Pos], unmarked: Map[Int, Pos], lastCalled: Int):
    def mark(num: Int): Board =
      if unmarked.contains(num) then
        Board(marked + (num -> unmarked(num)), unmarked - num, num)
      else
        this

    def score: Int = unmarked.keys.sum * lastCalled

    def wins: Boolean =
      val rows = marked.values.groupMapReduce(_._1)(_ => 1)(_ + _).exists(_._2 == 5)
      val cols = marked.values.groupMapReduce(_._2)(_ => 1)(_ + _).exists(_._2 == 5)
      rows || cols

    def playUntilWins(calledNumbers: List[Int]): (Board, Int) =
      calledNumbers
        .scanLeft(this)(_ mark _)
        .zipWithIndex
        .dropWhile(!_._1.wins)
        .head
  end Board
end Day4
