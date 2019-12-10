package advent2019
import common.Day
import common.Numeric.gcd
import scala.io.Source

class Day10(source: Source) extends Day {
  val grid = source.getLines.zipWithIndex.flatMap{case (line, y) => line.zipWithIndex.filter(_._1 == '#').map{case (_, x) => (x, y)}}.toSet
  val maxX = grid.map(_._1).max
  val maxY = grid.map(_._2).max

  def coordsAtDistance(x: Int, y: Int, distance: Int) =
    ((x - distance) to (x + distance)).flatMap(x => Set((x, y - distance), (x, y + distance))).toSet ++
    ((y - distance) to (y + distance)).flatMap(y => Set((x - distance, y), (x + distance, y))).toSet

  def blockers(x: Int, y: Int)(blocker: (Int, Int)): Set[(Int, Int)] = {
    val (blockerX, blockerY) = blocker
    val factor = math.abs(gcd(blockerX - x, blockerY - y))
    val dx = (blockerX - x) / factor
    val dy = (blockerY - y) / factor
    val xs = Iterator.from(blockerX, dx).drop(1).takeWhile(x => x >= 0 && x <= maxX)
    val ys = Iterator.from(blockerY, dy).drop(1).takeWhile(y => y >= 0 && y <= maxY)
    xs.zip(ys).toSet
  }

  @scala.annotation.tailrec
  final def visibility(from: (Int, Int), grid: Set[(Int, Int)], distance: Int = 1, count: Int = 0): Int = {
    val (x, y) = from
    if (x + distance > maxX && x - distance < 0 && y + distance > maxY && y - distance < 0)
      count
    else {
      val visibleAtDistance = grid & coordsAtDistance(x, y, distance)
      val blocked = visibleAtDistance.flatMap(blockers(x, y))
      visibility(from, grid -- blocked, distance + 1, count + visibleAtDistance.size)
    }
  }

  lazy val maxVisibility = grid.map(asteroid => (asteroid, visibility(asteroid, grid))).maxBy(_._2)
  lazy val (laserX, laserY) = maxVisibility._1

  def destructionOrder: List[(Int, Int)] = {
    val withoutLaser = grid.filter(_ != (laserX, laserY)).toList

    val groupedByExactAngles = withoutLaser.groupBy{case (x, y) =>
      val factor = math.abs(gcd(laserX - x, laserY - y))
      ((laserX - x) / factor, (laserY - y) / factor)
    }.values

    val sortedByDistance =
      groupedByExactAngles.toList
        .map{_.sortBy{case (x, y) => math.abs(laserX - x) + math.abs(laserY - y)}}

    val sortedByApproxAngle = sortedByDistance.sortBy{group =>
      val (x, y) = group.head
      val angle = math.atan2((laserY - y).toDouble, (x - laserX).toDouble)
      val rotated = math.Pi / 2.0 - angle
      val normalized = (4.0 * math.Pi + rotated) % (2.0 * math.Pi)
      normalized
    }

    val maxSize = sortedByApproxAngle.map(_.size).max
    val padded = sortedByApproxAngle.map(_.padTo(maxSize, (-1, -1)))
    val transposed = padded.transpose.map(_.filter(_ != (-1, -1)))
    transposed.flatten
  }

  override def answer1 = maxVisibility._2.toString

  override def answer2 = destructionOrder.drop(199).map{case (x, y) => x * 100 + y}.head.toString
}
