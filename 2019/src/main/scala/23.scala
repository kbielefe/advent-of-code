package day23

import cats.effect.{FiberIO, IO}
import cats.effect.kernel.{Deferred, Ref}
import cats.effect.std.Console
import cats.syntax.all.*
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] = for
    nics   <- forAllNics(i => IntCode(input).flatMap(Nic(i)))
    nat    <- Part1Nat(nics)
    fibers <- forAllNics(i => nics(i).start(nat))
    result <- nat.result
    _      <- fibers.traverse((x, y) => x.cancel *> y.cancel)
  yield result

  def part2(input: I): IO[Long] = for
    nics   <- forAllNics(i => IntCode(input).flatMap(Nic(i)))
    nat    <- Part2Nat(nics)
    fibers <- forAllNics(i => nics(i).start(nat))
    idle   <- nat.idleCheck.foreverM.start
    result <- nat.result
    _      <- idle.cancel
    _      <- fibers.traverse((x, y) => x.cancel *> y.cancel)
  yield result

case class Packet(destination: Long, x: Long, y: Long)

object Nic:
  def apply(networkAddress: Int)(computer: IntCode): IO[Nic] = for
    _   <- computer.input(networkAddress)
    ref <- Ref.of[IO, Int](0)
  yield new Nic(networkAddress, computer, ref)

class Nic private (networkAddress: Int, inputBlockedComputer: IntCode, receiveCountSinceSend: Ref[IO, Int]):
  val computer = inputBlockedComputer.onInputBlock(triedToReceive)

  def start(nat: Nat): IO[(FiberIO[Unit], FiberIO[Nothing])] =
    (computer.run.void.start, routePackets(nat).foreverM.start).tupled

  def triedToReceive: IO[Long] =
    Console[IO].println("Why does this help?") >>
    receiveCountSinceSend.update(_ + 1) >>
    IO.pure(-1)

  def send(packet: Packet): IO[Unit] =
    receiveCountSinceSend.set(0) >>
    computer.input(packet.x, packet.y)

  def idle: IO[Boolean] =
    (computer.inputIsEmpty, receivingWithoutSending).mapN(_ && _)

  def receivingWithoutSending: IO[Boolean] =
    receiveCountSinceSend.get.map(_ >= 100)

  def routePackets(nat: Nat): IO[Unit] = for
    destination <- computer.output
    x <- computer.output
    y <- computer.output
    _ <- nat.send(Packet(destination, x, y))
  yield ()

sealed trait Nat(deferred: Deferred[IO, Long]):
  def send(packet: Packet): IO[Unit]
  def result: IO[Long] = deferred.get

object Part1Nat:
  def apply(nics: Vector[Nic]): IO[Nat] =
    Deferred[IO, Long].map(new Part1Nat(nics, _))

object Part2Nat:
  def apply(nics: Vector[Nic]): IO[Part2Nat] = for
    sent     <- Ref.of[IO, Option[Packet]](None)
    received <- Ref.of[IO, Option[Packet]](None)
    deferred <- Deferred[IO, Long]
  yield new Part2Nat(nics, deferred, sent, received)

class Part1Nat private (nics: Vector[Nic], deferred: Deferred[IO, Long]) extends Nat(deferred):
  override def send(packet: Packet): IO[Unit] =
    if packet.destination == 255 then
      deferred.complete(packet.y).void
    else
      nics(packet.destination.toInt).send(packet)

class Part2Nat(nics: Vector[Nic], deferred: Deferred[IO, Long], lastPacketSent: Ref[IO, Option[Packet]], lastPacketReceived: Ref[IO, Option[Packet]]) extends Nat(deferred):
  override def send(packet: Packet): IO[Unit] =
    if packet.destination == 255 then
      lastPacketReceived.set(Some(packet))
    else
      nics(packet.destination.toInt).send(packet)

  def idleCheck: IO[Unit] = for
    idle     <- nics.traverse(_.idle).map(_.forall(identity))
    _        <- IO.whenA(idle)(forward)
  yield ()

  def forward: IO[Unit] = for
    received <- lastPacketReceived.get
    sent     <- lastPacketSent.get
    _        <- nics(0).send(received.get)
    _        <- lastPacketSent.set(received)
    _        <- IO.whenA(sentTwice(sent, received))(deferred.complete(sent.get.y).void)
  yield ()

  def sentTwice(sent: Option[Packet], received: Option[Packet]): Boolean =
    (sent.map(_.y) == received.map(_.y)) && sent.isDefined

def forAllNics[A](io: Int => IO[A]): IO[Vector[A]] =
  List.range(0, 50).parTraverse(io).map(_.toVector)
