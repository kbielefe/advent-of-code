package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day11 extends GridDay[Int, Int](2020, 11) {
  type Pos = (Int, Int)
  type SeatMap = Map[Pos, Char]
  type Neighbors = Map[Pos, List[Pos]]

  private def occupiedNeighbors(seats: SeatMap, pos: Pos, neighbors: Neighbors): Int =
    neighbors(pos).count(seats.getOrElse(_, ' ') == '#')

  private def applyRules(seats: SeatMap, neighbors: Map[Pos, List[Pos]], maxOccupancy: Int): SeatMap =
    seats.map{case (pos, char) =>
      if (char == 'L' && occupiedNeighbors(seats, pos, neighbors) == 0)
        (pos -> '#')
      else if (char == '#' && occupiedNeighbors(seats, pos, neighbors) >= maxOccupancy)
        (pos -> 'L')
      else
        (pos -> char)
    }.toMap

  private def firstSeen(from: Pos, dir: Pos, seats: SeatMap): Option[Pos] = {
    val (dirX, dirY) = dir
    val pos = Iterator
      .iterate(from){case (fromX, fromY) => (fromX + dirX, fromY + dirY)}
      .drop(1)
      .dropWhile(seats.getOrElse(_, ' ') == '.')
      .next()
    seats.get(pos).map(_ => pos)
  }

  private val dirs = List(-1 -> -1, -1 -> 0, -1 -> 1, 0 -> -1, 0 -> 1, 1 -> -1, 1 -> 0, 1 -> 1)

  private def solve(input: SeatMap, neighbors: Neighbors, maxOccupancy: Int): Task[Int] =
    Observable.unfold(input){seats =>
      val newSeats = applyRules(seats, neighbors, maxOccupancy)
      if (newSeats == seats)
        None
      else
        Some(newSeats.values.count(_ == '#') -> newSeats)
    }.lastL

  override def part1(input: SeatMap): Task[Int] = {
    val neighbors = input.flatMap{case ((fromX, fromY), char) =>
      if (char == 'L')
        Some((fromX, fromY) -> dirs.map{case (x, y) => (fromX + x, fromY + y)})
      else
        None
    }.toMap
    solve(input, neighbors, 4)
  }

  override def part2(input: SeatMap): Task[Int] = {
    val neighbors = input.flatMap{case (from, char) =>
      if (char == 'L')
        Some(from -> dirs.map(dir => firstSeen(from, dir, input)).flatten)
      else
        None
    }.toMap
    solve(input, neighbors, 5)
  }
}
