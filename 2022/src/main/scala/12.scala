package advent2022
import puzzleparse.{Grid, Pos}
import algorithms.AStar

object Day12:
  def part1(input: Grid[Char]): Int = answer(input, 1)
  def part2(input: Grid[Char]): Int = answer(input, 2)

  private def answer(input: Grid[Char], part: Int): Int =
    val startPos = input.find(_._2 == 'E').get._1
    val astar = new AStar[Pos, Int](goal(input, part), heuristic(input), neighborWeight, 0, getNeighbors(input))
    astar.getMinCost(startPos).get

  private def goal(grid: Grid[Char], part: Int)(pos: Pos): Boolean =
    if part == 1 then grid(pos) == 'S' else elevation(grid(pos)) == 0

  private def elevation(char: Char): Int =
    if char == 'S' then 0 else if char == 'E' then 25 else (char - 'a').toInt

  private def heuristic(grid: Grid[Char])(pos: Pos): Int = elevation(grid(pos))
  private def neighborWeight(from: Pos, to: Pos): Int = 1

  private def getNeighbors(grid: Grid[Char])(pos: Pos): Set[Pos] =
    val currentElevation = elevation(grid(pos))
    val potentialNewPos = Set(pos.copy(row = pos.row + 1), pos.copy(row = pos.row - 1), pos.copy(col = pos.col + 1), pos.copy(col = pos.col - 1))
    potentialNewPos.filter{newPos => grid.get(newPos) match
      case Some(char) => currentElevation - elevation(char) <= 1
      case None       => false
    }
