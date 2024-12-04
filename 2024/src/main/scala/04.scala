package day4
import algorithms.{Grid, given}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    val all =
      grid.findAll(4, "XMAS".r, List(_.east, _.west, _.north, _.south, _.north.east, _.north.west, _.south.east, _.south.west))
    all.size

  def part2(grid: Grid): Int =
    val up =
      grid.findAll(3, "MAS".r, List(_.north.east)).map(_.north.east) ++
      grid.findAll(3, "MAS".r, List(_.south.west)).map(_.south.west)
    val down =
      grid.findAll(3, "MAS".r, List(_.north.west)).map(_.north.west) ++
      grid.findAll(3, "MAS".r, List(_.south.east)).map(_.south.east)
    (up.toSet & down.toSet).size
