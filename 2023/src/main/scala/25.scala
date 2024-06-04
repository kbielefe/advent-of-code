package day25
import parse.{*, given}

type Component = String
case class Line(lhs: Component, rhs: List[Component]):
  def edges: Iterator[(Component, Component)] =
    Iterator.from(rhs).map(lhs -> _)

  def components: Set[Component] =
    rhs.toSet + lhs

type Lines = List[Line]
given Read[List[Component]] = Read(" ")
given Read[Line] = Read(": ")
given Read[Lines] = Read("\n")

object Puzzle extends runner.Day[Lines, Int, Int]:
  def part1(lines: Lines): Int =
    println(lines.toSet.flatMap(_.components).size)
    ???
