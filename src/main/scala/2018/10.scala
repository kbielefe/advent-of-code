package advent2018
import common.Day
import scala.io.Source

class Day10(source: Source) extends Day {

  case class Point(x: Long, y: Long, vx: Long, vy: Long)

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

  val points = source.getLines.toList map parsePoint

  def getAnimation(start: Long, interval: Long, points: List[Point]): Iterator[(Long, List[Point])] = {
    Iterator.iterate((start, advanceSeconds(points, start))){case (seconds, p) => (seconds + interval, advanceSeconds(p, interval))}
  }

  override def answer1: String = {
    val start: Long = 11000
    val interval: Long = 1
    val sizes = getAnimation(start, interval, points) map {_._2} map size
    val ((smallestSeconds, smallestPoints), (smallestSize, _)) = (getAnimation(start, interval, points).drop(2).zip(sizes zip sizes.drop(1))).dropWhile{case (points, (x, y)) => x > y}.next
    println(smallestSeconds)
    println(smallestSize)
    //println(message(smallest))
    "See printouts"
  }

  override def answer2: String = ???
}
