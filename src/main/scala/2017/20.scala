package advent2017
import common.Day
import scala.io.Source
import Math.{abs,sqrt,ceil}

class Day20(source: Source) extends Day {
  type Coord = List[Int]
  case class Particle(i: Int, p: Coord, v: Coord, a: Coord) {
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

  val input = source.getLines().zipWithIndex.map{case (line, index) => Particle(line, index)}.toList

  override def answer1 = input.minBy{_.a.map(abs).sum}.i.toString

  def steps = Iterator.iterate(input){input =>
    input.map(_.step).groupBy(_.p).filter(_._2.size == 1).values.flatten.toList
  }

  override def answer2 = steps.drop(100).next().size.toString
}
