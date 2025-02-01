package advent2021
import puzzleparse.{*, given}

object Day5:

  def part1(input: List[Line]): Int = countOverlaps(input.filter(_.straight))
  def part2(input: List[Line]): Int = countOverlaps(input)

  case class Line(x1: Nat, y1: Nat, x2: Nat, y2: Nat):
    def straight: Boolean = x1 == x2 || y1 == y2

    def points: Seq[Pos] =
      val xs = x1 to x2 by (if x1 > x2 then -1 else 1)
      val ys = y1 to y2 by (if y1 > y2 then -1 else 1)
      if straight then
        for
          x <- xs
          y <- ys
        yield Pos(x, y)
      else
        xs.zip(ys).map((x, y) => Pos(x, y))

  private def countOverlaps(lines: List[Line]): Int =
    lines.flatMap(_.points).groupBy(identity).count(_._2.size > 1)
