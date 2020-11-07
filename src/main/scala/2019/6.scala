package advent2019
import common.Day
import scala.io.Source
import scala.annotation.tailrec

class Day6(source: Source) extends Day {
  val orbits = source.getLines().map{line =>
    val array = line.split(')')
    (array(1), array(0))
  }.toMap

  val orbitsByParent = orbits.groupBy(_._2).view.mapValues(_.map(_._1))

  def totalDepth(node: String, depth: Int): Int = {
    val children = orbitsByParent.getOrElse(node, List.empty)
    depth + children.map(child => totalDepth(child, depth + 1)).sum
  }

  @tailrec
  final def pathTo(node: String, path: List[String] = List.empty): List[String] = {
    if (node == "COM")
      path
    else
      pathTo(orbits(node), node :: path)
  }

  def distanceToSanta: Int = {
    val pathToYou = pathTo("YOU")
    val pathToSanta = pathTo("SAN")
    val sharedDistance = pathToYou.zip(pathToSanta).takeWhile{case (x, y) => x == y}.length
    pathToYou.length + pathToSanta.length - sharedDistance * 2 - 2
  }

  override def answer1 = totalDepth("COM", 0).toString

  override def answer2 = distanceToSanta.toString
}
