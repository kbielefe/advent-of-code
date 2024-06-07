package day25
import algorithms.{Edge, Graph}
import parse.{*, given}
import visualizations.ForceGraph

type Component = String
type CGraph = Graph[Component, Unit]

case class Line(lhs: Component, rhs: List[Component]):
  def edges: Iterator[Edge[Component, Unit]] =
    Iterator.from(rhs).map(to => Edge(lhs, to, ()))

given Read[List[Component]] = Read(" ")
given Read[Line] = Read(": ")
given Read[CGraph] =
  Read[Iterator, Line]("\n")
    .map(lines => Graph.fromEdges(lines.flatMap(_.edges)))

object Puzzle extends runner.Day[CGraph, Int, Int]:
  def part1(graph: CGraph): Int =
    // Found by manual inspection of the visualization
    val cut = graph - Edge("ttj", "rpd", ()) - Edge("fqn", "dgc", ()) - Edge("vps", "htp", ())
    val bidirectional = cut.bidirectional
    val group1 = bidirectional.reachableFrom("ttj").vertices.size
    val group2 = bidirectional.reachableFrom("rpd").vertices.size
    group1 * group2

  def part2(graph: CGraph): Int = ???

  def browseGraph(graph: CGraph): Unit =
    ForceGraph.forGraph(graph)
