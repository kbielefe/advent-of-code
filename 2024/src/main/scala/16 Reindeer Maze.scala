package day16
import algorithms.{AStar, Grid, given}, Grid.{Pos, Dir, PosDir}
import cats.data.NonEmptyList
import parse.{*, given}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    astar(grid).getMinCost(PosDir(grid.find('S').get, Dir.East)).get

  def part2(grid: Grid): Int =
    val maxCost = part1(grid)
    val start = PosDir(grid.find('S').get, Dir.East)
    val goal = grid.find('E').get
    pathDFS(grid, start, Set.empty, 0, maxCost, goal, Map.empty)._2.size

  def pathDFS(grid: Grid, current: PosDir, path: Set[Pos], cost: Int, maxCost: Int, goal: Pos, minHere: Map[PosDir, Int]): (Map[PosDir, Int], Set[Pos]) =
    if cost > maxCost || grid.get(current.pos) == Some('#') || minHere.getOrElse(current, Int.MaxValue) < cost then
      (minHere, Set.empty)
    else if current.pos == goal then
      (minHere, path + goal)
    else
      val (minMove, resultMove)   = pathDFS(grid, current.moveForward, path + current.pos, cost + 1,    maxCost, goal, minHere + (current -> cost))
      val (minLeft, resultLeft)   = pathDFS(grid, current.turnLeft, path + current.pos,    cost + 1000, maxCost, goal, minMove)
      val (minRight, resultRight) = pathDFS(grid, current.turnRight, path + current.pos,   cost + 1000, maxCost, goal, minLeft)
      (minRight, resultLeft ++ resultRight ++ resultMove)

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
