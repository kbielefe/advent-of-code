package advent2021
import puzzleparse.{Read, given}

object Day17:
  def part1(target: Target): Int = target.highest
  def part2(target: Target): Int = target.hitCount

  case class Target(minX: Int, maxX: Int, minY: Int, maxY: Int) derives Read:
    def highest: Int = trajectories.filter(hits).flatMap(_.map(_._2)).max
    def hitCount: Int = trajectories.count(hits)

    private def trajectories: Iterator[List[(Int, Int)]] = for
      vx <- Iterator.range(1, maxX + 1)
      vy <- Iterator.range(minY, -minY + 1)
      xs = points(vx,  true).takeWhile(_ <= maxX)
      ys = points(vy, false).takeWhile(_ >= minY)
    yield xs.zip(ys).toList

    private def points(v: Int, max: Boolean): Iterator[Int] =
      def f(v: Int) = if max then Math.max(v, 0) else v
      Iterator.iterate((0, v))((a, v) => (a + v, f(v - 1))).map(_._1)

    private def hits(trajectory: List[(Int, Int)]): Boolean =
      trajectory.exists((x, y) => x >= minX && y <= maxY)
