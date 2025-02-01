package day17

import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.Stream
import parse.{*, given}
import scala.concurrent.duration.*
import year2019.IntCode

type I = Vector[Long]
given Read[I] = Read(",")

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] =
    ???

  def part2(input: I): IO[Long] = for
    computer <- IntCode(input.updated(0, 2L))
    _        <- computer.run.start
    fiber    <- putOutput(computer).start
    _        <- getInput(computer).foreverM.start
    result   <- fiber.joinWithNever
  yield result

  def putOutput(computer: IntCode): IO[Long] =
    Stream
      .repeatEval(computer.output)
      .takeThrough(_ <= 255)
      .evalTap(x => IO.whenA(x <= 255)(Console[IO].print(x.toChar)))
      .compile
      .lastOrError

  def getInput(computer: IntCode): IO[Unit] = for
    line <- Console[IO].readLine
    _    <- IO(assert(line.size <= 20))
    _    <- Console[IO].println("═" * 20)
    _    <- IO.unlessA(line.isEmpty)(computer.input((line + "\n").map(_.toLong)*))
  yield ()

/* Manual solution:
  Main = A,B,A,C,B,C,B,C,A,C

  A = R,12,L,10,R,12
  B = L,8,R,10,R,6
  C = R,12,L,10,R,10,L,8
*/
