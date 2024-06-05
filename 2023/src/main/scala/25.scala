package day25
import algorithms.{Edge, Graph}
import parse.{*, given}

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
    println(graph)
    ???

  def part2(graph: CGraph): Int = ???

  def graph(graph: CGraph): Unit =
    println("graph D {")
    graph.edges.map(edge => s"  ${edge.from} -- ${edge.to}").foreach(println)
    println("}")
