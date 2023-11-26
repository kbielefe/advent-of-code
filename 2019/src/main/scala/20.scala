package day20

import parse.given
import algorithms.{Grid, given}
import algorithms.Grid.Pos

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    println(input)
    val leftLabels = input.findAll(3, "[A-Z][A-Z]\\.".r).map(pos => (pos.east.east, s"${input(pos)}${input(pos.east)}"))
    val rightLabels = input.findAll(3, "\\.[A-Z][A-Z]".r).map(pos => (pos, s"${input(pos.east)}${input(pos.east.east)}", pos))
    val topLabels = input.findAllVertical(3, "[A-Z][A-Z]\\.".r).map(pos => (pos.south.south, s"${input(pos)}${input(pos.south)}"))
    val bottomLabels = input.findAllVertical(3, "\\.[A-Z][A-Z]".r).map(pos => (pos, s"${input(pos.south)}${input(pos.south.south)}"))
    val labels = leftLabels ++ rightLabels ++ topLabels ++ bottomLabels
    val labelByPos: Map[Pos, String] = labels.toMap
    val posByLabel: Map[String, Set[Pos]] = labels.groupBy(_._2).mapValues(_.map(_._1))
    posByLabel.foreach(println)
    labelByPos.foreach(println)
    ???

  def part2(input: Grid): Int =
    ???
