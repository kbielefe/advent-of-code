package day18
import algorithms.{AStar, Grid, given}, Grid.Pos
import parse.{*, given}

given Read[Pos] = Read[(Int, Int)](",").map((col, row) => Pos(row, col))
given Read[List[Pos]] = Read("\n")

object Puzzle extends runner.Day[List[Pos], Int, String]:
  def part1(bytes: List[Pos]): Int =
    val obstacles = bytes.take(1024).toSet
    astar(obstacles).getMinCost(Pos(0, 0)).get

  def part2(bytes: List[Pos]): String =
    val blocker = (0 until bytes.size).find: size =>
      !astar(bytes.take(size + 1).toSet).getMinCost(Pos(0, 0)).isDefined
    val pos = bytes(blocker.get)
    s"${pos.col},${pos.row}"

  def astar(obstacles: Set[Pos]): AStar[Pos, Int] =
    val goal = Pos(70, 70)
    new AStar[Pos, Int](
      _ == goal,
      _.manhattan(goal),
      (_, _) => 1,
      0,
      pos => Set(pos.north, pos.south, pos.east, pos.west).filter(pos => pos.row >= 0 && pos.row <= 70 && pos.col >= 0 && pos.col <= 70 && !obstacles.contains(pos))
    )
