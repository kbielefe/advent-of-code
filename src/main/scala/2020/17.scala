package advent2020

import common._

object Day17 extends SyncGridDay[Int, Int](2020, 17) {
  override def part1(input: Map[(Int, Int), Char]): Int =
    Iterator.iterate(parseInput(3, input))(conway).drop(6).next().size

  override def part2(input: Map[(Int, Int), Char]): Int =
    Iterator.iterate(parseInput(4, input))(conway).drop(6).next().size

  private def parseInput(dimensions: Int, input: Map[(Int, Int), Char]): Set[List[Int]] = {
    val fill = List.fill(dimensions - 2)(0)
    input.toList.filter(_._2 == '#').map(_._1).map{case (x, y) => List(x, y) ++ fill}.toSet
  }

  private def conway(cells: Set[List[Int]]): Set[List[Int]] =
    cells.flatMap(neighbors).flatMap{cell =>
      val activeNeighbors = neighbors(cell).count(neighbor => cells.contains(neighbor) && neighbor != cell)
      if (activeNeighbors == 3 || (activeNeighbors == 2 && cells.contains(cell)))
        Set(cell)
      else
        Set.empty
    }

  private def neighbors(cell: List[Int]): Iterator[List[Int]] =
    if (cell.isEmpty)
      Iterator(Nil)
    else {
      val head = cell.head
      neighbors(cell.tail).flatMap(tail => Iterator((head - 1) :: tail, head :: tail, (head + 1) :: tail))
    }
}
