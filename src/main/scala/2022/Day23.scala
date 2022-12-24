package advent2022
import puzzleparse.{Grid, Pos}

object Day23:
  type Elves = Set[Pos]
  def part1(input: Grid[Char]): Int =
    val elves = input.filter(_._2 == '#').keySet
    val after10Rounds = Iterator.iterate((elves, 0))(move.tupled).drop(10).next._1
    val minRow = after10Rounds.map(_.row).min
    val maxRow = after10Rounds.map(_.row).max
    val minCol = after10Rounds.map(_.col).min
    val maxCol = after10Rounds.map(_.col).max
    (maxCol - minCol + 1) * (maxRow - minRow + 1) - elves.size

  def part2(input: Grid[Char]): Int =
    val elves = input.filter(_._2 == '#').keySet
    Iterator.iterate((elves, 0))(move.tupled)
      .sliding(2)
      .dropWhile{case Seq(l, r) => l._1 != r._1}
      .next
      .apply(1)
      ._2

  private def move(elves: Elves, round: Int): (Elves, Int) =
    val proposals = elves.flatMap{case Pos(row, col) =>
      val neighbors = Set(
        Pos(row - 1, col - 1), Pos(row - 1, col), Pos(row - 1, col + 1),
        Pos(row,     col - 1),                    Pos(row,     col + 1),
        Pos(row + 1, col - 1), Pos(row + 1, col), Pos(row + 1, col + 1),
      )
      if (neighbors & elves).isEmpty then
        None
      else
        val emptyDirection = (0 to 3).iterator.map(i => "NSWE"((round + i) % 4)).find{direction =>
          val neighbors = direction match
            case 'N' => Set(Pos(row - 1, col - 1), Pos(row - 1, col), Pos(row - 1, col + 1))
            case 'S' => Set(Pos(row + 1, col - 1), Pos(row + 1, col), Pos(row + 1, col + 1))
            case 'W' => Set(Pos(row - 1, col - 1), Pos(row, col - 1), Pos(row + 1, col - 1))
            case 'E' => Set(Pos(row - 1, col + 1), Pos(row, col + 1), Pos(row + 1, col + 1))
          (neighbors & elves).isEmpty
        }
        emptyDirection match
          case Some('N') => Some(Pos(row, col) -> Pos(row - 1, col))
          case Some('S') => Some(Pos(row, col) -> Pos(row + 1, col))
          case Some('W') => Some(Pos(row, col) -> Pos(row, col - 1))
          case Some('E') => Some(Pos(row, col) -> Pos(row, col + 1))
          case None      => None
    }
    val uniqueProposals = proposals.groupBy(_._2).filter(_._2.size == 1).map(_._2.head)
    val from = uniqueProposals.keySet
    val to = uniqueProposals.values.toSet
    (elves -- from ++ to, round + 1)
