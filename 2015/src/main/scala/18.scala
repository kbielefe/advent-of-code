package day18
import parse.{*, given}
import algorithms.{*, given}
import algorithms.Grid.Pos

object Puzzle extends runner.Day[Grid, Int, Int]:
  given Grid.Neighbors = Grid.DiagonalNeighbors
  def rule(current: Char, neighborCount: Int): Char =
    if current == '#' then
      if neighborCount == 2 || neighborCount == 3 then
        '#'
      else
        '.'
    else
      if neighborCount == 3 then
        '#'
      else
        '.'

  def part1(input: Grid): Int =
    Iterator.iterate(input)(_.life(rule)).drop(100).next.count(_ == '#')

  def part2(input: Grid): Int =
    Iterator.iterate(stuckCorners(input))(grid => stuckCorners(grid.life(rule))).drop(100).next.count(_ == '#')

  def stuckCorners(grid: Grid): Grid =
    grid
      .updated(Pos(0,   0), '#')
      .updated(Pos(0,  99), '#')
      .updated(Pos(99,  0), '#')
      .updated(Pos(99, 99), '#')
