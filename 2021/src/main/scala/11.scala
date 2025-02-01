package advent2021
import puzzleparse.{Grid, Digit, Pos}
import scala.collection.MapView
import scala.annotation.tailrec

object Day11:
  def part1(input: Grid[Digit]): Int =
    Iterator.iterate(input.view, 101)(step).drop(1).map(flashCount).sum

  def part2(input: Grid[Digit]): Int =
    Iterator.iterate(input.view)(step).zipWithIndex.dropWhile(x => flashCount(x._1) != 100).next._2

  type Octopi = MapView[Pos, Int]

  private def step(octopi: Octopi): Octopi =
    (increaseByOne andThen propagateFlashes(Set.empty) andThen resetToZero)(octopi)

  private def increaseByOne(octopi: Octopi): Octopi =
    octopi.mapValues(_ + 1)

  @tailrec
  private def propagateFlashes(prevFlashed: Set[Pos])(octopi: Octopi): Octopi =
    val flashed = octopi.filter(_._2 > 9).map(_._1).toSet -- prevFlashed
    if flashed.isEmpty then
      octopi
    else
      val increasedNeighbors = flashed.toList.flatMap(_.neighbors(octopi)).groupBy(identity).mapValues(_.size)
      val newOctopi = octopi.map((pos, energy) => (pos, energy + increasedNeighbors.getOrElse(pos, 0))).toMap.view
      val newFlashed = flashed ++ prevFlashed
      propagateFlashes(newFlashed)(newOctopi)

  private def resetToZero(octopi: Octopi): Octopi =
    octopi.mapValues(x => if x > 9 then 0 else x)

  private def flashCount(octopi: Octopi): Int =
    octopi.count(_._2 == 0)

  extension(pos: Pos)
    def neighbors(octopi: Octopi): Set[Pos] =
      val result = for
        row <- (pos.row - 1) to (pos.row + 1)
        col <- (pos.col - 1) to (pos.col + 1)
      yield Pos(row, col)
      (result.toSet - pos).filter(octopi.contains)

