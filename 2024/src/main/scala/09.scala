package day9
import parse.{*, given}

object Puzzle extends runner.Day[String, Long, Long]:
  def part1(input: String): Long =
    val blocks = input.map(_.asDigit).zipWithIndex.flatMap: (digit, index) =>
      val isFile = index % 2 == 0
      val id = index / 2
      if isFile then List.fill(digit)(Some(id)) else List.fill(digit)(None)
    val totalFileSize = blocks.count(_.isDefined)
    val reversed = blocks.reverse.collect{case Some(id) => id}.toList
    compact(blocks.iterator, reversed).take(totalFileSize).zipWithIndex.map(_.toLong * _).sum
    
  def part2(input: String): Long =
    ???

  def compact(blocks: Iterator[Option[Int]], reversed: List[Int]): Iterator[Int] =
    blocks.takeWhile(_.isDefined).map(_.get) ++ Iterator(reversed.head) ++ compact(blocks, reversed.tail)
