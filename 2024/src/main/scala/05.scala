package day5
import parse.{*, given}
import algorithms.{Edge, Graph}
import scala.annotation.tailrec

type Edges = Vector[(Int, Int)]
case class Input(edges: Edges, updates: Vector[Vector[Int]])

given Read[(Int, Int)] = Read("""(\d+)\|(\d+)""".r)
given readEdges: Read[Edges] = Read("\n")
given readPages: Read[Vector[Int]] = Read(",")
given Read[Vector[Vector[Int]]] = Read("\n")
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Int, Int]:
  def part1(input: Input): Int =
    input.updates
      .filter(inOrder(input.edges))
      .map(middle)
      .sum

  def part2(input: Input): Int =
    input.updates
      .filterNot(inOrder(input.edges))
      .map(sort(input.edges))
      .map(middle)
      .sum

  def sort(edges: Edges)(update: Vector[Int]): Vector[Int] =
    val graphEdges = relevantEdges(edges, update)
      .map((from, to) => Edge(from, to, ()))
    Graph.fromEdges(graphEdges).toposort.toVector

  def middle(update: Vector[Int]): Int =
    update(update.size / 2)

  def relevantEdges(edges: Edges, update: Vector[Int]): Edges =
    edges.filter((before, after) => update.contains(before) && update.contains(after))

  def inOrder(edges: Edges)(update: Vector[Int]): Boolean =
    @tailrec
    def helper(edges: Edges, update: Vector[Int]): Boolean =
      if update.isEmpty then
        true
      else if edges.map(_._2).contains(update.head) then
        false
      else
        val newEdges = edges.filter((before, after) => before != update.head && after != update.head)
        helper(newEdges, update.tail)

    helper(relevantEdges(edges, update), update)
