package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

class Day16 extends DayTask[List[Int], String, String] {

  override def input(lines: Observable[String]) = lines.headL.map{_.map{_.asDigit}.toList}

  // digit starts at 1
  def pattern(digit: Int): Iterator[Int] = {
    Iterator.continually(1)
      .flatMap(_ => Iterator(0, 1, 0, -1))
      .flatMap(x => Iterator.fill(digit)(x))
      .drop(1)
  }

  def applyPattern(input: List[Int], phase: Int): List[Int] = {
    (1 to input.size).toList.map{digit => math.abs(pattern(digit).zip(input.iterator).map{case (x, y) => x * y}.sum) % 10}
  }

  override def part1(input: List[Int]) = Task{(1 to 100).foldLeft(input)(applyPattern).take(8).mkString}

  override def part2(input: List[Int]) = Task{"unimplemented"}
}
