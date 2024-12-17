package day16
import algorithms.{AStar, Grid, PriorityQueue, given}, Grid.{Pos, Dir, PosDir}
import cats.data.NonEmptyList
import parse.{*, given}
import scala.annotation.tailrec

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    astar(grid).getMinCost(PosDir(grid.find('S').get, Dir.East)).get

  def part2(grid: Grid): Int =
    val maxCost = part1(grid)
    val start = PosDir(grid.find('S').get, Dir.East)
    val goal = grid.find('E').get
    paths(PriorityQueue((start, Set(start.pos), 0), 0), goal, grid, Iterator.empty, maxCost).flatten.size

  @tailrec
  def paths(queue: PriorityQueue[(PosDir, Set[Pos], Int), Int], goal: Pos, grid: Grid, accum: Iterator[Set[Pos]], maxCost: Int): Iterator[Set[Pos]] =
    queue.dequeue match
      case None => accum
      case Some((posDir, path, cost) -> rest) if posDir.pos == goal => paths(rest, goal, grid, accum ++ Iterator(path + posDir.pos), maxCost)
      case Some((posDir, path, cost) -> rest) =>
        val neighbors = List(
          (posDir.turnLeft,    path + posDir.pos, cost - 1000) -> (cost - 1000),
          (posDir.turnRight,   path + posDir.pos, cost - 1000) -> (cost - 1000),
          (posDir.moveForward, path + posDir.pos, cost - 1)    -> (cost - 1)
        ).filter(n => -n._2 < maxCost && grid(n._1._1.pos) != '#' && !path.contains(n._1._1.pos))
        paths(rest.enqueue(neighbors), goal, grid, accum, maxCost)

  def getNeighbors(grid: Grid)(current: PosDir): Set[PosDir] =
    Set(
      current.turnLeft,
      current.turnRight,
      current.moveForward
    ).filter(posDir => grid(posDir.pos) != '#')

  def astar(grid: Grid): AStar[PosDir, Int] =
    val goal = grid.find('E').get
    def neighborWeight(current: PosDir, neighbor: PosDir): Int =
      if current.dir != neighbor.dir then 1000 else 1

    new AStar[PosDir, Int](
      _.pos == goal,
      _.pos.manhattan(goal),
      neighborWeight,
      0,
      getNeighbors(grid)
    )
