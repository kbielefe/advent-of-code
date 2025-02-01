package day24

import parse.{*, given}
import algorithms.{*, given}
import algorithms.Grid.Pos

given Grid.Neighbors = Grid.NSEWNeighbors

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    def grids = Iterator.iterate(input)(life)
    val Some(x -> _) = detectCycle(grids, 0): @unchecked
    val firstRepeated = grids.drop(x).next
    biodiversityRating(firstRepeated)

  def part2(input: Grid): Int =
    val bugs = input.filter(_ == '#').allPos.map(pos => (0, pos))
    val result = Iterator.iterate(bugs)(plutonianLife).drop(200).next
    result.size

  def biodiversityRating(grid: Grid): Int =
    val positions = for
      row <- (grid.minRow to grid.maxRow).iterator
      col <- (grid.minCol to grid.maxCol).iterator
    yield Grid.Pos(row, col)
    val powersOfTwo = Iterator.iterate(1)(_ * 2)
    positions.zip(powersOfTwo).map((pos, power) => if grid(pos) == '#' then power else 0).sum

  def life(grid: Grid): Grid =
    grid.allPos.foldLeft(grid){(accum, pos) =>
      val current = grid(pos)
      val count = grid.neighborCount(pos)
      val newChar = if current == '#' && count != 1 then
        '.'
      else if current == '.' && (count == 1 || count == 2) then
        '#'
      else
        current
      accum.updated(pos, newChar)
    }

  def plutonianLife(bugs: Set[(Int, Pos)]): Set[(Int, Pos)] =
    (bugs.flatMap(neighbors) ++ bugs).foldLeft(bugs){case (accum, pos) =>
      val count = neighbors(pos).count(bugs.contains)
      if bugs.contains(pos) && count != 1 then
        accum - pos
      else if !bugs.contains(pos) && (count == 1 || count == 2) then
        accum + pos
      else
        accum
    }

  def neighbors(bug: (Int, Pos)): Set[(Int, Pos)] =
    val (level, p) = bug
    val outer = level - 1
    val inner = level + 1
    val outerRowNeighbors =
      if p.row == 0 then
        Set(outer -> Pos(1, 2))
      else if p.row == 4 then
        Set(outer -> Pos(3, 2))
      else
        Set.empty
    val outerColNeighbors =
      if p.col == 0 then
        Set(outer -> Pos(2, 1))
      else if p.col == 4 then
        Set(outer -> Pos(2, 3))
      else
        Set.empty
    val innerNeighbors =
      if p == Pos(1, 2) then
        (0 to 4).toSet.map(col => (inner -> Pos(0, col)))
      else if p == Pos(2, 1) then
        (0 to 4).toSet.map(row => (inner -> Pos(row, 0)))
      else if p == Pos(3, 2) then
        (0 to 4).toSet.map(col => (inner -> Pos(4, col)))
      else if p == Pos(2, 3) then
        (0 to 4).toSet.map(row => (inner -> Pos(row, 4)))
      else
        Set.empty
    val levelNeighbors =
      p.neighbors.filter(p => p.row >= 0 && p.row <= 4 && p.col >= 0 && p.col <= 4).map(p => (level -> p)) - (level -> Pos(2, 2))
    outerRowNeighbors ++ outerColNeighbors ++ innerNeighbors ++ levelNeighbors

  def bugString(bugs: Set[(Int, Pos)]): String =
    val minLevel = bugs.map(_._1).min
    val maxLevel = bugs.map(_._1).max
    (minLevel to maxLevel).map{level =>
      s"Depth $level\n" +
      (0 to 4).map{row =>
        (0 to 4).map{col =>
          if ((row, col)) == (2, 2) then
            '?'
          else if bugs.contains((level -> Pos(row, col))) then
            '#'
          else
            '.'
        }.mkString
      }.mkString("\n")
    }.mkString("\n\n")
