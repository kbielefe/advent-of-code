package day5
import cats.effect.IO
import fs2.Stream
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] = for
    computer <- IntCode(input)
    _        <- computer.input(1)
    fiber    <- computer.run.start
    result   <- Stream.repeatEval(computer.output).find(_ != 0).compile.onlyOrError
    _        <- fiber.cancel
  yield result

  def part2(input: I): IO[Long] = for
    computer <- IntCode(input)
    _        <- computer.input(5)
    _        <- computer.run
    result   <- computer.output
  yield result
