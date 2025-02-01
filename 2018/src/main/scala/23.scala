package advent2018

import common._

object Day23 extends SyncStringsDay[Long, Long](2018, 23) {
  private case class Nanobot(x: Long, y: Long, z: Long, r: Long) {
    def overlaps(other: Nanobot): Boolean = {
      Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z) <= (r + other.r)
    }
    def distanceFromOrigin: Long =
      Math.abs(x) + Math.abs(y) + Math.abs(z) - r
  }

  private def parse(input: Seq[String]): Seq[Nanobot] =
    input.map{line =>
      val s"pos=<$x,$y,$z>, r=$r" = line
      Nanobot(x.toLong, y.toLong, z.toLong, r.toLong)
    }

  override def part1(input: Seq[String]): Long =
    ???

  override def part2(input: Seq[String]): Long = {
    val bots = parse(input)
    val neighbors = calculateBotNeighbors(bots)
    val maximalClique = bronKerbosch(Set.empty, bots.toSet, Set.empty, neighbors)
    maximalClique.get.map(_.distanceFromOrigin).max
  }

  private def calculateBotNeighbors(bots: Seq[Nanobot]): Map[Nanobot, Set[Nanobot]] =
    bots.combinations(2).foldLeft(Map.empty[Nanobot, Set[Nanobot]]){case (neighbors, Seq(a, b)) =>
      if (a overlaps b) {
        neighbors + (a -> (neighbors.getOrElse(a, Set.empty) ++ Set(b))) + (b -> (neighbors.getOrElse(b, Set.empty) ++ Set(a)))
      } else {
        neighbors
      }
    }

  private def bronKerbosch(r: Set[Nanobot], p: Set[Nanobot], x: Set[Nanobot], neighbors: Map[Nanobot, Set[Nanobot]]): Option[Set[Nanobot]] =
    if (p.isEmpty && x.isEmpty)
      Some(r)
    else {
      val pivot = p.union(x).head
      val vs = p -- neighbors(pivot)
      val recursiveResults = vs.iterator.scanLeft[(Set[Nanobot], Set[Nanobot], Option[Set[Nanobot]])]((p, x, None)){case ((p, x, _), v) =>
        (p - v, x + v, bronKerbosch(r + v, p.intersect(neighbors(v)), x.intersect(neighbors(v)), neighbors))
      }.dropWhile(_._3.isEmpty)
      if (recursiveResults.hasNext)
        recursiveResults.next()._3
      else
        None
    }
}
