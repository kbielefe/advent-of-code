package day23
import algorithms.{Edge, Graph}
import algorithms.{Grid, given}
import algorithms.Grid.{*, given}
import parse.{*, given}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import visualizations.ForceGraph, ForceGraph.LinkDirectionalParticles

object Puzzle extends runner.Day[Grid, Int, Int]:
  given Neighbors = NSEWNeighbors

  def part1(grid: Grid): Int =
    answer(grid)

  def part2(grid: Grid): Int =
    val startCol = grid.row(grid.minRow).indexWhere(_ == '.')
    val goalCol  = grid.row(grid.maxRow).indexWhere(_ == '.')
    val start = Pos(grid.minRow, startCol)
    val goal  = Pos(grid.maxRow, goalCol)
    val initial = State(start, start, -1)
    val graph = buildGraph(Graph.empty, goal, grid, Set.empty, Queue(initial))
    val firstEdge = graph.outgoingEdges(start)
    val modified = modifyOutsideEdges(graph, goal, Queue.from(firstEdge))
    modified.combinedPaths(start, goal).max

  val browseGraph = ForceGraph(title = Some("Advent of Code [2023 Day 23]"), description = "A simplified graph showing the intersections"): (grid: Grid) =>
    val startCol = grid.row(grid.minRow).indexWhere(_ == '.')
    val goalCol  = grid.row(grid.maxRow).indexWhere(_ == '.')
    val start = Pos(grid.minRow, startCol)
    val goal  = Pos(grid.maxRow, goalCol)
    val initial = State(start, start, -1)
    buildGraph(Graph.empty, goal, grid, Set.empty, Queue(initial))

  case class State(position: Pos, lastIntersection: Pos, distance: Int)

  @tailrec
  def buildGraph(graph: Graph[Pos, Int], goal: Pos, grid: Grid, visited: Set[(Pos, Pos)], queue: Queue[State]): Graph[Pos, Int] =
    queue.dequeueOption match
      case None =>
        graph
      case Some((State(pos, last, distance), remaining)) if isIntersection(pos, grid) || pos == goal =>
        val neighbors = pos
          .neighbors
          .filter(grid.contains)
          .filterNot(grid(_) == '#')
          .filterNot(pos => visited.contains(pos -> last))
          .map(next => State(next, pos, 0))
        val edge = Edge(last, pos, distance + 1)
        val newGraph = if last == pos then graph else graph + edge
        buildGraph(newGraph, goal, grid, visited + (pos -> last), remaining ++ neighbors)
      case Some((State(pos, last, distance), remaining)) =>
        val neighbors = pos
          .neighbors
          .filter(grid.contains)
          .filterNot(grid(_) == '#')
          .filterNot(pos => visited.contains(pos -> last))
          .map(pos => State(pos, last, distance + 1))
        buildGraph(graph, goal, grid, visited + (pos -> last), remaining ++ neighbors)

  @tailrec
  def modifyOutsideEdges(graph: Graph[Pos, Int], goal: Pos, queue: Queue[Edge[Pos, Int]]): Graph[Pos, Int] =
    val nextToLast = graph.incomingEdges(goal).head.from
    queue.dequeueOption match
      case None => graph
      case Some((edge, remaining)) =>
        val newEdges = graph.outgoingEdges(edge.to).filter(edge => graph.outgoingEdges(edge.to).size < 4) - edge.reverse
        val newGraph = if edge.from == nextToLast then graph else graph - edge.reverse
        modifyOutsideEdges(newGraph, goal, remaining ++ newEdges)

  def isIntersection(pos: Pos, grid: Grid): Boolean =
    pos
      .neighbors
      .filter(grid.contains)
      .filterNot(grid(_) == '#')
      .size > 2

  private def answer(grid: Grid): Int =
    val startCol = grid.row(grid.minRow).indexWhere(_ == '.')
    val goalCol  = grid.row(grid.maxRow).indexWhere(_ == '.')
    val start = Pos(grid.minRow, startCol)
    val goal  = Pos(grid.maxRow, goalCol)
    longestPath(grid, goal, 0, Queue((start, 0, Set.empty)))

  @tailrec
  private def longestPath(grid: Grid, goal: Pos, max: Int, queue: Queue[(Pos, Int, Set[Pos])]): Int =
    queue.dequeueOption match
      case None => max
      case Some(((pos, length, visited), remaining)) =>
        if pos == goal then
          longestPath(grid, goal, Math.max(max, length), remaining)
        else
          longestPath(grid, goal, max, remaining ++ neighbors(grid, pos).filterNot(visited.contains).map(n => (n, length + 1, visited + pos)))

  private def neighbors(grid: Grid, pos: Pos): Set[Pos] =
    grid(pos) match
      case '>' => Set(pos.east)
      case '<' => Set(pos.west)
      case '^' => Set(pos.north)
      case 'v' => Set(pos.south)
      case _   => pos.neighbors.filter(grid.contains).filterNot(grid(_) == '#')
