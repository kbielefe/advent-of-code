import scala.io.Source
import Math.abs

type Coord = Array[Int]
case class Particle(i: Int, p: Coord, v: Coord, a: Coord) {
  //at^2 + bt + c = 0
  //(-b (+-) sqrt(b^2 - 4ac))/2a
  //bt + c = 0
  //t = -c/b
  def collisions(other: Particle): Set[Int] = ???
}

object Particle {
  def apply(line: String, index: Int): Particle = {
    val Array(p, v, a) = line.split(", ").map(_.drop(3).dropRight(1).split(",").map(_.toInt))
    new Particle(index, p, v, a)
  }
}

val input = Source.fromFile("input20.txt").getLines.zipWithIndex.map{case (line, index) => Particle(line, index)}

val answer1 = input.minBy{_.a.map(abs).sum}.i
println(answer1)
