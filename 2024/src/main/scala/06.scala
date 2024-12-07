package day6
import algorithms.{Grid, addMulti, given}, Grid.{Dir, Pos}
import cats.effect.IO
import fs2.Stream
import visualizations.*

object Puzzle extends runner.IODay[Grid, Int, Int]:
  case class Guard(pos: Pos, dir: Dir)
  def part1(grid: Grid): IO[Int] =
    findGuardPath(grid).map(_._2.keySet.size).compile.lastOrError

  def part2(grid: Grid): IO[Int] =
    findGuardPath(grid).map(_._3).compile.lastOrError

  def findGuardPath(grid: Grid): Stream[IO, (Guard, Map[Pos, Set[Dir]], Int)] =
    val guard = Guard(grid.find('^').get, Dir.North)
    val moves = Stream.iterate((guard, Map.empty[Pos, Set[Dir]], 0)): (guard, visited, obstacleCount) =>
      val moveForward = guard.pos.moveInDir(guard.dir)
      val blocked = grid.get(moveForward) == Some('#')
      val newGuard = if blocked then Guard(guard.pos, guard.dir.turnRight) else Guard(moveForward, guard.dir)
      val newVisited = visited.addMulti(guard.pos, guard.dir)
      val newObstacleCount = if blocked then obstacleCount else if alreadyVisitedToRight(guard, grid, visited) then obstacleCount + 1 else obstacleCount
      (newGuard, newVisited, newObstacleCount)
    moves.takeThrough((guard, _, _) => grid.contains(guard.pos))

  def alreadyVisitedToRight(guard: Guard, grid: Grid, visited: Map[Pos, Set[Dir]]): Boolean =
    Iterator.iterate(guard.pos)(_.moveInDir(guard.dir.turnRight))
      .takeWhile(grid.contains)
      .takeWhile(grid.get(_) != Some('#'))
      .exists(pos => visited.getOrElse(pos, Set.empty).contains(guard.dir.turnRight))

  val animation = AnimatedGrid(title="2024 Day 6 Part 2"): (grid: Grid) =>
    val frames = findGuardPath(grid).zipWithIndex.sliding(2).map: pair =>
      val guard = pair(0)._1._1
      val index = pair(0)._2
      val foundObstacle = pair(0)._1._3 < pair(1)._1._3
      Frame(Map((guard.pos.row, guard.pos.col) -> Seq(ChangeChar(guard.dir.arrow), ChangeColor(if foundObstacle then "green" else "red"), ChangeTooltip(index.toString))))
    (grid, frames)
  end animation
