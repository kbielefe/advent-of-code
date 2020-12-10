package advent2018

import common._

object Day23 extends SyncStringsDay[Long, Long](2018, 23) {
  private case class Nanobot(x: Long, y: Long, z: Long, r: Long)

  private def parse(input: Seq[String]): Seq[Nanobot] =
    input.map{line =>
      val s"pos=<$x,$y,$z>, r=$r" = line
      Nanobot(x.toLong, y.toLong, z.toLong, r.toLong)
    }

  override def part1(input: Seq[String]): Long =
    ???

  override def part2(input: Seq[String]): Long = {
    val bots = parse(input)
    0
  }
  //75197225 is too low

  /*
   * Equation for a point that is in range of one bot:
   * |px - ax| + |py - ay| + |pz - az| <= ar
   * Equation for a point that is in range of two bots:
   * |px - ax| + |py - ay| + |pz - az| <= ar
   * |px - bx| + |py - by| + |pz - bz| <= br
   *
   *  Get it into conjunctive normal form and use a SAT solver?
   *  Research taxicab geometry.
   */
}
