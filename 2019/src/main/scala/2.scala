package day2

import cats.effect.IO
import fs2.Stream
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long]
given Read[I] = Read(",")

object Puzzle extends runner.IODay[I, Long, Int]:
  def part1(input: I): IO[Long] =
    IntCode(input.updated(1, 12L).updated(2, 2L)).flatMap(_.run.map(_(0)))

  def part2(input: I): IO[Int] =
    val stream = for
      noun     <- Stream.range(0, 100)
      verb     <- Stream.range(0, 100)
      computer <- Stream.eval(IntCode(input.updated(1, noun.toLong).updated(2, verb.toLong)))
      matches  <- Stream.eval(computer.run.map(_(0) == 19690720))
    yield (matches, 100 * noun + verb)

    stream.find(_._1).map(_._2).compile.onlyOrError
