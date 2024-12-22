package day22
import parse.{*, given}

given Read[List[Long]] = Read("\n")
object Puzzle extends runner.Day[List[Long], Long, Long]:
  def part1(initial: List[Long]): Long =
    initial.map(secret => secrets(secret).drop(2000).next).sum

  def part2(initial: List[Long]): Long =
    initial
      .flatMap(sequences)
      .groupMapReduce(_._1)(_._2)(_ + _)
      .map(_._2)
      .max

  def secrets(secret: Long): Iterator[Long] =
    Iterator.iterate(secret)(mixAndPrune(_ * 64) andThen mixAndPrune(_ / 32) andThen mixAndPrune(_ * 2048))

  def mixAndPrune(f: Long => Long)(secret: Long): Long =
    (f(secret) ^ secret) % 16777216

  def sequences(secret: Long): List[(Seq[Long], Long)] =
    secrets(secret)
      .take(2001)
      .map(_ % 10)
      .sliding(5)
      .toList
      .groupMapReduce(_.sliding(2).map{case Seq(l, r) => r - l}.toSeq)(_.last)((x, y) => x)
      .toList
