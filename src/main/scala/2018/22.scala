package advent2018
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

class Day22 extends DayTask[(Int, (Int, Int)), Int, Int] {

  override def input(lines: Observable[String]) = Task{(10914, (9, 739))}

  val Rocky  = 0
  val Wet    = 1
  val Narrow = 2

  def geologicIndex(above: Int, left: Int, x: Int, y: Int, targetX: Int, targetY: Int): Int = (x, y) match {
    case (0, 0)                 => 0
    case (`targetX`, `targetY`) => 0
    case (x, 0)                 => x * 16807
    case (0, y)                 => y * 48271
    case (x, y)                 => above * left
  }

  def erosion(depth: Int, targetX: Int, targetY: Int): Seq[Seq[Int]] = {
    val initialAbove = Seq.fill(targetX + 1)(1)
    (0 to targetY).scanLeft(initialAbove){case (rowAbove, y) =>
      (0 to targetX).scanLeft(1){case (left, x) =>
        val above = rowAbove(x)
        (geologicIndex(above, left, x, y, targetX, targetY) + depth) % 20183
      }.drop(1)
    }.drop(1)
  }

  def regionTypes(erosion: Seq[Seq[Int]]): Seq[Seq[Int]] =
    erosion.map(_.map(_ % 3))

  override def part1(input: (Int, (Int, Int))) = Task{
    val (depth, (targetX, targetY)) = input
    regionTypes(erosion(depth, targetX, targetY)).map(_.sum).sum
  }

  override def part2(input: (Int, (Int, Int))) = Task{
    val (depth, (targetX, targetY)) = input
    0
  }
}
