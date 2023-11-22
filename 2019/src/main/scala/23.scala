package day23

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.std.Console
import cats.syntax.all.*
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] = for
    deferred  <- Deferred[IO, Long]
    computers <- IntCode(input, blocking = false).parReplicateA(50).map(_.toVector)
    _              <- List.range(0, 50).parTraverse(index => computers(index).input(index))
    computerFibers <- List.range(0, 50).parTraverse(index => computers(index).run.start)
    routerFibers   <- List.range(0, 50).parTraverse(index => routePackets(computers, deferred, computers(index)).foreverM.start)
    result         <- deferred.get
    _ <- computerFibers.parTraverse(_.cancel)
    _ <- routerFibers.parTraverse(_.cancel)
  yield result

  def part2(input: I): IO[Long] =
    ???

  def routePackets(computers: Vector[IntCode], deferred: Deferred[IO, Long], computer: IntCode): IO[Unit] = for
    destination <- computer.output
    x <- computer.output
    y <- computer.output
    _ <- IO.whenA(destination >= 0 && destination < 50)(computers(destination.toInt).input(x, y))
    _ <- IO.whenA(destination == 255)(deferred.complete(y).void)
  yield ()
