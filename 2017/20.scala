import scala.io.Source
import Math.abs

type Coord = (Int, Int, Int)
case class Particle(p: Coord, v: Coord, a: Coord)
object Particle {
  def apply(line: String): Particle = {
    val Array(p, v, a) = line.split(", ").map(_.drop(3).dropRight(1).split(",").map(_.toInt)).map{case Array(x, y, z) => (x, y, z)}
    new Particle(p, v, a)
  }
}
val input = Source.fromFile("input20.txt").getLines.map(Particle(_))

val answer1 = input.zipWithIndex.minBy{x => val a = x._1.a; abs(a._1) + abs(a._2) + abs(a._3)}._2
println(answer1)
