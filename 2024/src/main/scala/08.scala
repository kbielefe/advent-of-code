package day8
import algorithms.{Grid, given}, Grid.Pos

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    val frequencies = grid.charSet - '.'
    frequencies.flatMap(antinodes(grid)).size

  def part2(grid: Grid): Int =
    val frequencies = grid.charSet - '.'
    frequencies.flatMap(resonantAntinodes(grid)).size

  def antinodes(grid: Grid)(frequency: Char): Set[Pos] =
    grid.findAll(frequency)
      .toList
      .combinations(2)
      .flatMap:
        case Seq(left, right) =>
          Seq(Pos(left.row - (right.row - left.row), left.col - (right.col - left.col)),
              Pos(right.row - (left.row - right.row), right.col - (left.col - right.col)))
            .filter(grid.contains)
        case _ => ???
      .toSet

  def resonantAntinodes(grid: Grid)(frequency: Char): Set[Pos] =
    grid.findAll(frequency)
      .toList
      .combinations(2)
      .flatMap:
        case Seq(left, right) =>
          val lefts = Iterator.iterate(left): pos =>
              Pos(pos.row - (right.row - left.row), pos.col - (right.col - left.col))
            .takeWhile(grid.contains)
          val rights = Iterator.iterate(right): pos =>
              Pos(pos.row - (left.row - right.row), pos.col - (left.col - right.col))
            .takeWhile(grid.contains)
          lefts ++ rights
        case _ => ???
      .toSet
