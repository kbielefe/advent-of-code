package day21

import algorithms.QuineMcCluskey
import algorithms.QuineMcCluskey.Minterm
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
  def part1(input: I): IO[Long] = for
    computer <- IntCode(input)
    fiber    <- putOutput(computer).start
    _        <- (computer.run >> IO.sleep(2.seconds) >> fiber.cancel).start
    _        <- getInput(computer).foreverM.onError(_ => fiber.cancel).start
    result   <- fiber.joinWithNever
  yield result
/*
OR A J
AND B J
AND C J
NOT J J
AND D J
WALK
*/

  def part2(input: I): IO[Long] =
    val (dontCares, nonDontCares) = (0 to 511).partition(isDontCare)
    val oneTerms = nonDontCares.filter(validJumpSpot)

    QuineMcCluskey("ABCDEFGHI", oneTerms.map(Minterm(_)).toSet, dontCares.map(Minterm(_)).toSet)
    ???

  def isDontCare(minterm: Int): Boolean =
    val string = mintermString(minterm)
    // Anything with '....' is a don't care because it won't appear in the input
    string.contains("....")

  def validJumpSpot(minterm: Int): Boolean =
    validJumpSpot(mintermString(minterm))

  def mintermString(minterm: Int): String =
    Minterm(minterm).toBitSeq.takeRight(9).map(bit => if bit == 1 then '#' else '.').mkString

  def validJumpSpot(example: String): Boolean =
    !example.startsWith("####") && (example.size < 4 || (example(3) == '#' && validJumpOrMoveSpot(example.drop(4))))

  def validJumpOrMoveSpot(example: String): Boolean =
    validJumpSpot(example) || example.isEmpty || (example.head == '#' && validJumpOrMoveSpot(example.tail))
/*
To Set T or J to equal A at start:
OR A T

To Set T or J to equal A at not start:
NOT A T
NOT T T

To Set T or J to equal !A (any time):
NOT A T

A'DEF + A'DEI + A'DH + B'DEF + B'DEI + B'DH + C'DEF + C'DEI + C'DH (from Quine McCluskey)
A'(DEF + DEI + DH) + B'(DEF + DEI + DH) + C'(DEF + DEI + DH)
(DEF + DEI + DH)(A' + B' + C')
(D(E(F + I) + H))(A' + B' + C')
(D(E(F + I) + H))(ABC)'

If there's a hole in A, B, or C
and you can jump twice
or jump, move once, then jump
or jump and move twice

OR F J
OR I J
AND E J
OR H J
AND D J
OR A T
AND B T
AND C T
NOT T T
AND T J
RUN
*/

  def putOutput(computer: IntCode): IO[Long] =
    Stream
      .repeatEval(computer.output)
      .takeThrough(_ <= 255)
      .evalTap(x => IO.whenA(x <= 255)(Console[IO].print(x.toChar)))
      .compile
      .lastOrError

  def getInput(computer: IntCode): IO[Unit] = for
    line <- Console[IO].readLine
    _    <- computer.input((line + "\n").map(_.toLong)*)
  yield ()
