package advent2016
import algorithms.AStar
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day13:
  private def heuristic(start: (Int, Int)): Int =
    Math.abs(start._1 - goal._1) + Math.abs(start._2 - goal._2)

  private def getNeighbors(input: Int)(start: (Int, Int)): Set[(Int, Int)] =
    val (x, y) = start
    Set((x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1))
      .filter{case (x, y) => x >= 0 && y >= 0}
      .filter(openSpace(input))

  private def openSpace(input: Int)(pos: (Int, Int)): Boolean =
    val (x, y) = pos
    val number = x*x + 3*x + 2*x*y + y + y*y + input
    (0 to 32).map(x => (number >> x) & 0x01).sum % 2 == 0

  private def edgeWeight(start: (Int, Int), end: (Int, Int)): Int = 1

  @tailrec
  private def bfs(input: Int, open: Queue[((Int, Int), Int)], visited: Set[(Int, Int)]): Int =
    if open.isEmpty then
      visited.size
    else
      val (((x, y), depth), newOpen) = open.dequeue
      val newVisited = visited + ((x, y))
      val neighbors = getNeighbors(input)((x, y)) -- newVisited
      val withDepth = neighbors.map(n => (n, depth + 1))
      if depth < 50 then
        bfs(input, newOpen.enqueueAll(withDepth), newVisited)
      else
        bfs(input, newOpen, newVisited)

  val goal = (31, 39)

  def part1(input: Int): Int =
    val astar = new AStar[(Int, Int), Int](_ == goal, heuristic, edgeWeight, 0, getNeighbors(input))
    astar.getMinCost((1, 1)).get

  def part2(input: Int): Int =
    bfs(input, Queue(((1, 1), 0)), Set.empty)
