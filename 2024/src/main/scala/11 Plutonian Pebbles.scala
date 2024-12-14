package day11
import parse.{*, given}

given Read[List[Long]] = Read(" ")
object Puzzle extends runner.Day[List[Long], Long, Long]:
  def part1(stones: List[Long]): Long =
    stoneCountAfterBlinks(25, stones)

  def part2(stones: List[Long]): Long =
    stoneCountAfterBlinks(75, stones)

  def stoneCountAfterBlinks(blinks: Int, stones: List[Long]): Long =
    val initial = stones.groupMapReduce(identity)(_ => 1L)(_ + _)
    Iterator.iterate(initial)(blink).drop(blinks).next.values.sum

  def blink(stonesMap: Map[Long, Long]): Map[Long, Long] =
    stonesMap.toList.flatMap((stone, count) => blink(stone).map(_ -> count)).groupMapReduce(_._1)(_._2)(_ + _)

  def blink(stone: Long): Iterator[Long] =
    val stoneString = stone.toString
    val digits = stoneString.size
    if stone == 0 then
      Iterator(1)
    else if digits % 2 == 0 then
      Iterator(stoneString.take(digits / 2).toLong, stoneString.drop(digits / 2).toLong)
    else
      Iterator(stone * 2024)
