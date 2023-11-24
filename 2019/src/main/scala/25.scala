package day25

import cats.effect.IO
import cats.effect.std.{Console, Supervisor}
import cats.syntax.all.*
import fs2.Stream
import parse.{*, given}
import scala.concurrent.duration.*
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.IODay[I, Long, Long]:
  def part1(input: I): IO[Long] =
    Supervisor[IO].use { supervisor =>
      for
        computer <- IntCode(input)
        ofiber   <- putOutput(computer).supervise(supervisor)
        ifiber   <- getInput(computer).foreverM.supervise(supervisor)
        _        <- (computer.run >> ifiber.cancel >> ofiber.cancel).supervise(supervisor)
        result   <- ofiber.joinWithNever
      yield result
    }

  def part2(input: I): IO[Long] = ???

  def putOutput(computer: IntCode): IO[Long] =
    Stream
      .repeatEval(computer.output)
      .takeThrough(_ <= 255)
      .evalTap(x => IO.whenA(x <= 255)(Console[IO].print(x.toChar)))
      .compile
      .lastOrError

  var coords = ""

  def myOwnCommands(line: String): IO[Unit] = line match
    case "east"   => IO{if !coords.isEmpty && coords.last == 'W' then coords = coords.dropRight(1) else coords = coords + "E"} >> printCoords
    case "west"   => IO{if !coords.isEmpty && coords.last == 'E' then coords = coords.dropRight(1) else coords = coords + "W"} >> printCoords
    case "north"  => IO{if !coords.isEmpty && coords.last == 'S' then coords = coords.dropRight(1) else coords = coords + "N"} >> printCoords
    case "south"  => IO{if !coords.isEmpty && coords.last == 'N' then coords = coords.dropRight(1) else coords = coords + "S"} >> printCoords
    case "failed" => IO{coords = "ENWNWWS"} >> printCoords
    case _ => printCoords

  def printCoords: IO[Unit] = Console[IO].println(coords)

  def isValid(line: String): Boolean =
    List("east", "west", "north", "south", "inv", "take", "drop").contains(line.split(" ")(0))

  def getInput(computer: IntCode): IO[Unit] = for
    line <- Console[IO].readLine
    _    <- myOwnCommands(line)
    _    <- IO.whenA(isValid(line))(computer.input((line + "\n").map(_.toLong)*))
  yield ()

  val dangerousItems = Set("photons", "giant electromagnet", "molten lava", "infinite loop", "escape pod")
  // Coordinates are path dependent. It's not a grid.
  //
  // To explore:
  //
  // Visited:
  // Hull Breach
  // E Crew Quarters
  // EN Warp drive maintenance
  // ENW Science lab
  // ENWN Arcade (whirled peas)
  // ENWNW Storage
  // ENWNWW Hot Chocolate fountain (astronaut ice cream)
  // ENWNWWS Security Checkpoint
  // ENWNWWSS Pressure sensitive floor
  // S Hallway
  // SE Passages
  // SW Observatory
  // W Holodeck (hypercube)
  // WW Stables (space law space brochure)
  // WWW Sick Bay
  // WWWW Navigation (escape pod bad at early point)
  // WWWN Gift Wrapping Center (shell)
  // WWWNW Engineering (mug)
  // WWWNWS Kitchen (festive hat)
  // WS corridor
  //
  // correct weight: hypercube, festive hat, shell, astronaut ice cream
