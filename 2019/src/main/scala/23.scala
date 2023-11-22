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
    computers <- IntCode(input, blocking = false).parReplicateA(50).map(_.toVector)
    deferred  <- Deferred[IO, Long]
    _         <- List.range(0, 50).parTraverse(nic(computers, deferred))
    result    <- deferred.get
  yield result

  def part2(input: I): IO[Long] =
    ???

  def nic(computers: Vector[IntCode], deferred: Deferred[IO, Long])(index: Int): IO[Unit] = for
    computer <- IO.pure(computers(index))
    _ <- computer.input(index)
    _ <- computer.run.start
    _ <- routePackets(computers, deferred, index).foreverM.start
  yield ()

  def routePackets(computers: Vector[IntCode], deferred: Deferred[IO, Long], index: Int): IO[Unit] = for
    computer    <- IO.pure(computers(index))
    destination <- computer.output
    x <- computer.output
    y <- computer.output
    _ <- Console[IO].println(s"$index: $destination $x $y")
    _ <- IO.whenA(destination >= 0 && destination < 50)(computers(destination.toInt).input(x, y))
    _ <- IO.whenA(destination == 255)(deferred.complete(y).void)
  yield ()
