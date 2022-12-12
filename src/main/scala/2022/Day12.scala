package advent2022
import puzzleparse.{Grid, Pos}
import algorithms.AStar

object Day12:
  def part1(input: Grid[Char]): Int =
    val startPos = input.find(_._2 == 'S').get._1
    val goalPos = input.find(_._2 == 'E').get._1
    val astar = new AStar[Pos, Int](goal(input, 1), heuristic(input, goalPos), neighborWeight, 0, getNeighbors(input, 1))
    astar.getMinCost(startPos).get

  def part2(input: Grid[Char]): Int =
    val startPos = input.find(_._2 == 'E').get._1
    val astar = new AStar[Pos, Int](goal(input, 2), heuristicPart2(input), neighborWeight, 0, getNeighbors(input, 2))
    astar.getPath(startPos).foreach(println)
    astar.getMinCost(startPos).get

  private def goal(grid: Grid[Char], part: Int)(pos: Pos): Boolean =
    if part == 1 then grid(pos) == 'E' else elevation(grid(pos)) == 0

  private def elevation(char: Char): Int =
    if char == 'S' then 0 else if char == 'E' then 25 else (char - 'a').toInt

  private def heuristic(grid: Grid[Char], goalPos: Pos)(pos: Pos): Int =
    Math.max(Math.abs(goalPos.row - pos.row) + Math.abs(goalPos.col - pos.col), 25 - elevation(grid(pos)))

  private def heuristicPart2(grid: Grid[Char])(pos: Pos): Int =
    elevation(grid(pos))

  private def neighborWeight(from: Pos, to: Pos): Int = 1

  private def getNeighbors(grid: Grid[Char], part: Int)(pos: Pos): Set[Pos] =
    val currentElevation = elevation(grid(pos))
    val potentialNewPos = Set(pos.copy(row = pos.row + 1), pos.copy(row = pos.row - 1), pos.copy(col = pos.col + 1), pos.copy(col = pos.col - 1))
    potentialNewPos.filter{newPos => grid.get(newPos) match
      case Some(char) => (if part == 1 then elevation(char) - currentElevation else currentElevation - elevation(char)) <= 1
      case None       => false
    }
