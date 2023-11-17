package runner

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.implicits.given
import com.monovore.decline.*
import com.monovore.decline.effect.*

/** Creates the main function with CLI options for a puzzle year.
 *  To use, create a scala file containing `object Main extends AdventYear(<year>)`
 */
trait AdventYear(year: Int):
  private val day = Opts.argument[Int](metavar = "day")
  private val part = Opts.argument[Int](metavar = "part")
  private val answerArg = Opts.argument[String](metavar = "answer").orNone
  private val example = Opts.option[String]("example", short = "e", metavar = "example name", help = "Use the given example input instead of the official input.").orNone
  private val common = (Opts(year), day, part, example)

  private val run = Opts.subcommand("run", "Run the specified puzzle.")(common.mapN(RunPuzzle.apply))
  private val input = Opts.subcommand("input", "Copy the puzzle's input from the clipboard.")((Opts(year), day, example).mapN(Input.apply))
  private val answer = Opts.subcommand("answer", "Specify the correct answer for the puzzle. Copies from the clipboard by default.")((Opts(year), day, part, example, answerArg).mapN(Answer.apply))
  private val correct = Opts.subcommand("correct", "Mark the currently calculated answer as correct.")(common.mapN(Correct.apply))
  private val incorrect = Opts.subcommand("incorrect", "Mark the currently calculated answer as incorrect.")(common.mapN(Incorrect.apply))
  private val high = Opts.subcommand("high", "Mark the currently calculated answer as too high.")(common.mapN(High.apply))
  private val low = Opts.subcommand("low", "Mark the currently calculated answer as too low.")(common.mapN(Low.apply))
  private val session = Opts.subcommand("session", "Copy the user's session cookie from the clipboard.")(Opts(Session()))
  private val database = Opts.subcommand("database", "Initialize the database at advent.db.")(Opts(InitDatabase()))

  private val all = run `orElse` input `orElse` answer `orElse` correct `orElse` incorrect `orElse` high `orElse` low `orElse` session `orElse` database

  private val opts = all.map(_.run.as(ExitCode.Success))

  def main(args: Array[String]): Unit =
    CommandIOApp
      .run[IO]("run", s"$year Advent of Code Runner")(opts, args.toList)
      .unsafeRunSync()
