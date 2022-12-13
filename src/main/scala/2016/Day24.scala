package advent2016
import algorithms.AStar
import puzzleparse.{Grid, Pos}

object Day24:
  def part1(input: Grid[Char]): Int =
    val distances = (0 to 7).combinations(2).map(distance(input)).toMap
    (1 to 7).permutations.map(order => totalDistance(distances, 0 +: order)).min

  def part2(input: Grid[Char]): Int =
    val distances = (0 to 7).combinations(2).map(distance(input)).toMap
    (1 to 7).permutations.map(order => totalDistance(distances, 0 +: order :+ 0)).min

  private def distance(grid: Grid[Char])(pair: IndexedSeq[Int]): ((Int, Int), Int) =
    val IndexedSeq(from, to) = pair: @unchecked
    val IndexedSeq(fromPos, toPos) = pair.map(char => grid.find(_._2 == (char + '0')).get._1): @unchecked
    val astar = new AStar[Pos, Int](_ == toPos, heuristic(toPos), (_, _) => 1, 0, getNeighbors(grid))
    (from, to) -> astar.getMinCost(fromPos).get

  private def totalDistance(distances: Map[(Int, Int), Int], order: IndexedSeq[Int]): Int =
    order.sliding(2).map(_.sorted).map{case IndexedSeq(from, to) => distances((from, to))}.sum

  private def heuristic(goal: Pos)(current: Pos): Int =
    Math.abs(goal.row - current.row) + Math.abs(goal.col - current.col)

  private def getNeighbors(grid: Grid[Char])(current: Pos): Set[Pos] =
    Set(current.copy(row = current.row + 1), current.copy(row = current.row - 1), current.copy(col = current.col + 1), current.copy(col = current.col - 1))
      .filter(grid.contains)
      .filter(grid(_) != '#')
