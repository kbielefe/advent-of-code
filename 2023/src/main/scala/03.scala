package day3
import parse.{*, given}
import algorithms.{Grid, given}
import Grid.{Group, Pos}

given Grid.Neighbors = Grid.DiagonalNeighbors

extension (g: Grid)
  def allLabels: List[Int] =
    allLabelPos.map(_.string.toInt)

  def allLabelPos: List[Group] =
    val groups = g.findGroups(_.isDigit)
    groups.filter(adjacentSymbol(_).isDefined)

  def adjacentSymbol(group: Group): Option[Pos] =
    group.neighbors.filter(g.contains).find(pos => !g(pos).isDigit && g(pos) != '.')

  def allGearRatios: List[Int] =
    val gears = allLabelPos.groupBy(adjacentSymbol).filter(_._2.size >= 2)
    gears.toList.map(_._2).map(ratio)

  def ratio(labels: List[Group]): Int =
    labels.map(_.string.toInt).product

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    input.allLabels.sum

  def part2(input: Grid): Int =
    input.allGearRatios.sum
