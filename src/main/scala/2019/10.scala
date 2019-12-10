package advent2019
import common.Day
import common.Numeric.gcd
import scala.io.Source

class Day10(source: Source) extends Day {
  val grid = source.getLines.zipWithIndex.flatMap{case (line, y) => line.zipWithIndex.filter(_._1 == '#').map{case (_, x) => (x, y)}}.toSet

  final def visibility(from: (Int, Int), grid: Set[(Int, Int)]): Int = {
    val (x, y) = from
    groupedByExactAngles(grid, x, y).size
  }

  def groupedByExactAngles(grid: Set[(Int, Int)], fromX: Int, fromY: Int): Iterable[Set[(Int, Int)]] = {
    grid.filter(_ != (fromX, fromY)).groupBy{case (x, y) =>
      val factor = math.abs(gcd(fromX - x, fromY - y))
      ((fromX - x) / factor, (fromY - y) / factor)
    }.values
  }

  lazy val maxVisibility = grid.map(asteroid => (asteroid, visibility(asteroid, grid))).maxBy(_._2)
  lazy val (laserX, laserY) = maxVisibility._1

  def destructionOrder: List[(Int, Int)] = {
    val sortedByDistance = groupedByExactAngles(grid, laserX, laserY)
        .map{_.toList.sortBy{case (x, y) => math.abs(laserX - x) + math.abs(laserY - y)}}

    val sortedByApproxAngle = sortedByDistance.toList.sortBy{group =>
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
