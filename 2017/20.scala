import scala.io.Source
import Math.{abs,sqrt,ceil}

type Coord = List[Int]
case class Particle(i: Int, p: Coord, v: Coord, a: Coord) {

  private def maxCoordCollisionTime(i: Int, other: Particle): Double = {
    val da = (a(i) - other.a(i)).toDouble
    val dv = (v(i) - other.v(i)).toDouble
    val dp = (p(i) - other.p(i)).toDouble
    if (da == 0.0) {
      if (dv == 0.0) 0.0 else (-1 * dp / dv)
    } else {
      val discriminant = dv * dv - 4.0 * da * dp
      if (discriminant >= 0.0) {
        (-1.0 * dv + sqrt(discriminant)) / (2.0 * da)
      } else
        0.0
    }
  }

  def maxCollisionTime(other: Particle): Double = {
    (0 to 2).toList.map(i => maxCoordCollisionTime(i, other)).max
  }

  override def toString: String =
    s"Particle($i, ${p.mkString("(", ", ", ")")}, ${v.mkString("(", ", ", ")")}, ${a.mkString("(", ", ", ")")})"

  def step: Particle = {
    val newV = (0 to 2).map(i => v(i) + a(i)).toList
    val newP = (0 to 2).map(i => newV(i) + p(i)).toList
    Particle(i, newP, newV, a)
  }
}

object Particle {
  def apply(line: String, index: Int): Particle = {
    val Array(p, v, a) = line.split(", ").map(_.drop(3).dropRight(1).split(",").map(_.toInt).toList)
    new Particle(index, p, v, a)
  }
}

val input = Source.fromFile("input20.txt").getLines.zipWithIndex.map{case (line, index) => Particle(line, index)}.toList

val answer1 = input.minBy{_.a.map(abs).sum}.i
println(answer1)

def numberOfSteps = ceil(input.combinations(2).map{case List(x, y) => x.maxCollisionTime(y)}.max).toInt

def steps = Iterator.iterate(input){input =>
  input.map(_.step).groupBy(_.p).filter(_._2.size == 1).values.flatten.toList
}

val answer2 = steps.drop(numberOfSteps).next.size
println(answer2)
