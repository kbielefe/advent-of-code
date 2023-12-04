package day23

import parse.{*, given}
import cats.data.State
import cats.syntax.all.*

type Registers = Map[String, Long]

def modify(register: String, f: Long => Long): State[Registers, Unit] =
  State.modify(s => s.updated(register, f(s(register))))

def jumpIf(register: String, p: Long => Boolean, offset: Int): State[Registers, Unit] = for
  r <- State.inspect[Registers, Long](_(register))
  j  = if p(r) then offset.toInt else 1
  _ <- modify("pc", _ + j)
yield ()

val getPc = State.inspect[Registers, Int](_("pc").toInt)

def runInstruction(input: Vector[State[Registers, Unit]]): State[Registers, Unit] =
  getPc.flatMap(input)

val printState = State.inspect[Registers, Unit](println)

given Read[State[Registers, Unit]] with
  def read(in: String): State[Registers, Unit] = in match
    case s"hlf $r"          => modify(r, _ / 2) >> modify("pc", _ + 1)
    case s"tpl $r"          => modify(r, _ * 3) >> modify("pc", _ + 1)
    case s"inc $r"          => modify(r, _ + 1) >> modify("pc", _ + 1)
    case s"jmp $offset"     => modify("pc", _ + offset.toInt)
    case s"jie $r, $offset" => jumpIf(r, _ % 2 == 0, offset.toInt)
    case s"jio $r, $offset" => jumpIf(r, _ == 1, offset.toInt)

type I = Vector[State[Registers, Unit]] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    (runInstruction(input) >> getPc)
      .iterateWhile(_ < input.size)
      .runS(Map.empty.withDefault(_ => 0))
      .value("b")

  def part2(input: I): Long =
    (runInstruction(input) >> getPc)
      .iterateWhile(_ < input.size)
      .runS(Map("a" -> 1L).withDefault(_ => 0))
      .value("b")
