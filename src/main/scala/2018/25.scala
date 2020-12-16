package advent2018

import common._
import scala.annotation.tailrec

object Day25 extends SyncStringsDay[Int, Int](2018, 25) {
  type Point = Array[Int]
  type Graph = Map[Point, Set[Point]]

  override def part1(input: Seq[String]): Int = {
    val points = input.map(_.split(",").map(_.toInt))
    val graph = getGraph(points)
    val constellations = getConstellations(graph)
    constellations.size
  }

  override def part2(input: Seq[String]): Int = ???

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

  private def getConstellations(graph: Graph): Set[Set[Point]] =
    ???

  private def getConstellation(graph: Graph, start: Point, constellation: Set[Point]): Set[Point] =
    ???
}
