package advent2018

import common._
import scala.annotation.tailrec

object Day25 extends SyncStringsDay[Int, String](2018, 25) {
  type Point = Array[Int]
  type Graph = Map[Point, Set[Point]]

  override def part1(input: Seq[String]): Int = {
    val points = input.map(_.split(",").map(_.toInt))
    val graph = getGraph(points)
    val constellations = getConstellations(graph, points.toSet, Set.empty)
    constellations.size
  }

  override def part2(input: Seq[String]): String = "Free"

  private def distance(a: Point, b: Point): Int =
    a.zip(b).map{case (x, y) => Math.abs(x - y)}.sum

  private def getGraph(points: Seq[Point]): Graph =
    points
      .combinations(2)
      .flatMap{case Seq(a, b) =>
        if (distance(a, b) <= 3) Seq((a, b), (b, a)) else Seq.empty
      }
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap

  private def pointToString(point: Point): String = point.mkString("(", ", ", ")")

  @tailrec
  private def getConstellations(graph: Graph, next: Set[Point], acc: Set[Set[Point]]): Set[Set[Point]] = {
    if (next.isEmpty)
      acc
    else {
      val head = next.head
      val constellation = getConstellation(graph, Set(head), Set.empty)
      getConstellations(graph, next - head -- constellation, acc + constellation)
    }
  }

  @tailrec
  private def getConstellation(graph: Graph, next: Set[Point], constellation: Set[Point]): Set[Point] = {
    if (next.isEmpty)
      constellation
    else {
      val head = next.head
      val neighbors = graph.getOrElse(head, Set.empty)
      getConstellation(graph, next - head ++ neighbors -- constellation, constellation + head)
    }
  }
}
