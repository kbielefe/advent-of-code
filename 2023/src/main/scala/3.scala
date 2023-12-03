package day3
import parse.{*, given}
import algorithms.{Grid, given}
import Grid.Pos

given Grid.Neighbors = Grid.DiagonalNeighbors

extension (g: Grid)
  def allLabels: List[Int] =
    allLabelPos.map(getInt)

  def allLabelPos: List[Pos] =
    val allPos = (1 to 3).toSet.flatMap(g.findAll(_, "\\d+".r)).filterNot(p => g.contains(p.west) && g(p.west).isDigit)
    allPos.toList.filter(adjacentSymbol(_).isDefined)

  def adjacentSymbol(pos: Pos): Option[Pos] =
    val digitPositions = Set(pos, pos.east, pos.east.east).filter(g.contains).filter(g(_).isDigit)
    digitPositions.flatMap(_.neighbors).filter(g.contains).find(pos => !g(pos).isDigit && g(pos) != '.')

  def getInt(pos: Pos): Int =
    List(pos, pos.east, pos.east.east).filter(g.contains).map(g(_)).takeWhile(_.isDigit).mkString.toInt

  def allGearRatios: List[Int] =
    val gears = allLabelPos.groupBy(adjacentSymbol).filter(_._2.size >= 2)
    gears.toList.map(_._2).map(ratio)

  def ratio(labels: List[Pos]): Int =
    labels.map(getInt).product

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    input.allLabels.sum

  def part2(input: Grid): Int =
    input.allGearRatios.sum
