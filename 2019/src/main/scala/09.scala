package day9
import cats.effect.IO
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long]
given Read[I] = Read(",")

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] = for
    computer <- IntCode(input)
    _        <- computer.input(1)
    _        <- computer.run
    result   <- computer.output
  yield result

  def part2(input: I): IO[Long] =
    ???
