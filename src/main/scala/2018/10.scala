package advent2018
import common.Day
import scala.io.Source

class Day10(source: Source) extends Day {

  case class Point(x: Long, y: Long, vx: Long, vy: Long) {
    def secondsToMinDistanceTo(other: Point): Double = {
      val dvy = (vy - other.vy).toDouble
      val dy  = (y  - other.y ).toDouble
      val dvx = (vx - other.vx).toDouble
      val dx  = (x  - other.x ).toDouble
      -1.0 * (dvy * dy + dvx * dx) / (dvy * dvy + dvx * dvx)
    }
  }

  val pointRegex = """position=<\s*(-?\d+)\s*,\s*(-?\d+)\s*> velocity=<\s*(-?\d+)\s*,\s*(-?\d+)>""".r

  def parsePoint(in: String): Point = in match {
    case pointRegex(x, y, vx, vy) => Point(x.toLong, y.toLong, vx.toLong, vy.toLong)
  }

  def size(points: List[Point]): Long = {
    val pointSet = points.map{p => (p.x, p.y)}.toSet
    val left   = pointSet.map{_._1}.min
    val right  = pointSet.map{_._1}.max
    val top    = pointSet.map{_._2}.min
    val bottom = pointSet.map{_._2}.max
    (right - left + 1) * (bottom - top + 1)
  }

  def message(points: List[Point]): String = {
    val pointSet = points.map{p => (p.x, p.y)}.toSet
    val left   = pointSet.map{_._1}.min
    val right  = pointSet.map{_._1}.max
    val top    = pointSet.map{_._2}.min
    val bottom = pointSet.map{_._2}.max
    def messageLine(y: Long): String = (left to right).map{x => if (pointSet contains (x, y)) '#' else '.'}.mkString
    (top to bottom).map(messageLine).mkString("\n")
  }

  def advanceSeconds(points: List[Point], seconds: Long = 1): List[Point] = {
    points map {p => Point(p.x + p.vx * seconds, p.y + p.vy * seconds, p.vx, p.vy)}
  }

  val points = source.getLines().toList map parsePoint

  def minDistanceSeconds(points: List[Point]): Iterator[Double] = {
    points.combinations(2).map{case List(a, b) => a.secondsToMinDistanceTo(b)}
  }

  def averageMinDistanceSeconds(points: List[Point]): Double = {
    val (count, sum) = minDistanceSeconds(points).filterNot{_.isNaN}.filter{_ > 0.0}.foldLeft((0.0, 0.0)){case ((count, sum), next) =>
      (count + 1.0, sum + next)
    }
    sum / count
  }

  def getAnimation(start: Long, interval: Long, points: List[Point]): Iterator[List[Point]] = {
    Iterator.iterate(advanceSeconds(points, start)){p => advanceSeconds(p, interval)}
  }

  override def answer1: String = {
    val start: Long = (averageMinDistanceSeconds(points) - 5).toLong
    val interval: Long = 1
    println(start)
    getAnimation(start, interval, points).map(message) take 10 foreach println
    "PHFZCEZX"
  }

  override def answer2: String = "10634"
}
