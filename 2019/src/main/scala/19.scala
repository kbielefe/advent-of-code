package day19

import cats.effect.IO
import cats.effect.std.Console
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] =
    ???

  def part2(input: I): IO[Long] =
    IntCode(input)
      .flatMap(solve(0, 0, true))
      .map((x, y) => x * 10000 + y)

  def solve(x: Long, y: Long, increasingX: Boolean)(computer: IntCode): IO[(Long, Long)] = for
    bottomLeftHits <- computer.run.start >> computer.input(x)    >> computer.input(y+99) >> computer.output.map(_ == 1)
    topRightHits   <- computer.run.start >> computer.input(x+99) >> computer.input(y)    >> computer.output.map(_ == 1)
    result <- (bottomLeftHits, topRightHits, increasingX) match
      case (true, true, _)   => IO.pure((x, y))
      case (true,  _, true)  => solve(x, y + 1, false)(computer)
      case (false, _, true)  => solve(x + 1, y, true)(computer)
      case (_,  true, false) => solve(x + 1, y, true)(computer)
      case (_, false, false) => solve(x, y + 1, false)(computer)
  yield result
