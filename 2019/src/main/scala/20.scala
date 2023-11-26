package day20

import parse.given
import algorithms.{AStar, Grid, given}
import algorithms.Grid.Pos

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    val (labelByPos, posByLabel) = getLabels(input)
    val start = posByLabel("AA").head
    val end = posByLabel("ZZ").head
    def getNeighbors(pos: Pos): Set[Pos] =
      val warps = labelByPos.get(pos) match
        case Some(label) => posByLabel(label) - pos
        case None => Set.empty
      pos.neighbors.filter(input(_) == '.') ++ warps
    val astar = new AStar[Pos, Int](_ == end, _ => 0, (_, _) => 1, 0, getNeighbors)
    astar.getMinCost(start).get

  def part2(input: Grid): Int =
    val (labelByPos, posByLabel) = getLabels(input)
    val start = (posByLabel("AA").head, 0)
    val end = (posByLabel("ZZ").head, 0)
    def getNeighbors(posAndLevel: (Pos, Int)): Set[(Pos, Int)] =
      val (pos, level) = posAndLevel
      val warps = labelByPos.get(pos) match
        case Some(label) if isOuter(input, pos) && level > 0 => (posByLabel(label) - pos).map(_ -> (level - 1))
        case Some(label) if !isOuter(input, pos) => (posByLabel(label) - pos).map(_ -> (level + 1))
        case _ => Set.empty
      pos.neighbors.filter(input(_) == '.').map(_ -> level) ++ warps
    val astar = new AStar[(Pos, Int), Int](_ == end, _ => 0, (_, _) => 1, 0, getNeighbors)
    astar.getMinCost(start).get

  def isOuter(grid: Grid, pos: Pos): Boolean =
    pos.row == 2 ||
    pos.row == grid.maxRow - 2 ||
    pos.col == 2 ||
    pos.col == grid.maxCol - 2

  def getLabels(input: Grid): (Map[Pos, String], Map[String, Set[Pos]]) =
    val leftLabels = input.findAll(3, "[A-Z][A-Z]\\.".r).map(pos => (pos.east.east -> s"${input(pos)}${input(pos.east)}"))
    val rightLabels = input.findAll(3, "\\.[A-Z][A-Z]".r).map(pos => (pos -> s"${input(pos.east)}${input(pos.east.east)}"))
    val topLabels = input.findAllVertical(3, "[A-Z][A-Z]\\.".r).map(pos => (pos.south.south -> s"${input(pos)}${input(pos.south)}"))
    val bottomLabels = input.findAllVertical(3, "\\.[A-Z][A-Z]".r).map(pos => (pos -> s"${input(pos.south)}${input(pos.south.south)}"))
    val labels = leftLabels ++ rightLabels ++ topLabels ++ bottomLabels
    val labelByPos = labels.toMap
    val posByLabel = labels.groupBy(_._2).view.mapValues(_.map(_._1)).toMap
    (labelByPos, posByLabel)
