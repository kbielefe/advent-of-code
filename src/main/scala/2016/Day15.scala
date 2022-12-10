package advent2016

object Day15:
  def part1(input: List[String]): Int =
    val parsed = input.map{
      case s"Disc #$_ has $x positions; at time=0, it is at position $y." => (x.toInt, y.toInt)
    }
    Iterator.from(0).find(time => parsed.zipWithIndex.forall{case ((x, y), i) => (i + y + time + 1) % x == 0}).get

  def part2(input: List[String]): Int =
    val parsed = input.map{
      case s"Disc #$_ has $x positions; at time=0, it is at position $y." => (x.toInt, y.toInt)
    } :+ (11, 0)
    Iterator.from(0).find(time => parsed.zipWithIndex.forall{case ((x, y), i) => (i + y + time + 1) % x == 0}).get
