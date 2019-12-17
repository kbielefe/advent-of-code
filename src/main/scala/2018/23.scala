package advent2018
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

case class Nanobot(x: Long, y: Long, z: Long, r: Long) {
  def withinRangeOf(other: Nanobot): Boolean = {
    val distance = math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)
    distance <= other.r
  }
}

class Day23 extends DayTask[List[Nanobot], Int, String] {

  val regex = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  override def input(lines: Observable[String]) = lines.map{_ match {
      case regex(x, y, z, r) => Nanobot(x.toLong, y.toLong, z.toLong, r.toLong)
    }
  }.toListL

  override def part1(nanobots: List[Nanobot]) = Task{
    val strongest = nanobots.maxBy(_.r)
    nanobots.count(_.withinRangeOf(strongest))
  }

  override def part2(nanobots: List[Nanobot]) = Task{
    "unimplemented"
  }
}
