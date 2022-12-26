package advent2022
import datastructures.Circular

object Day20:
  def part1(input: Vector[Long]): Long = answer(input, 1, 1)

  def part2(input: Vector[Long]): Long = answer(input, 811589153, 10)

  private def answer(input: Vector[Long], decryptionKey: Long, count: Int): Long =
    def mix(circular: Circular[(Long, Int)]): Circular[(Long, Int)] =
      (0 until input.size).iterator.foldLeft(circular){(circular, pos) =>
        val index = circular.indexWhere(_._2 == pos)
        val number = circular(index)._1
        circular.move(index, index + number)
      }
    val result = Iterator.iterate(new Circular(input.map(_ * decryptionKey).zipWithIndex))(mix).drop(count).next.map(_._1)
    val index = result.indexOf(0)
    val first = result(index + 1000)
    val second = result(index + 2000)
    val third = result(index + 3000)
    first + second + third
