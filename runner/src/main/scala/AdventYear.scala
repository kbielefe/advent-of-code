package runner

import cats.effect.*
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.implicits.given
import cats.syntax.all.given
import com.monovore.decline.*
import com.monovore.decline.effect.*

/** Creates the main function with CLI options for a puzzle year.
 *  To use, create a scala file containing `object Main extends AdventYear(<year>)`
 */
trait AdventYear(year: Int):
  private val day = Opts.argument[Int](metavar = "day")
    .validate("Day must be between 1 and 25.")(day => day >= 1 && day <= 25)
  private val part = Opts.argument[Int](metavar = "part")
    .validate("Part must be 1 or 2.")(part => part == 1 || part == 2)
  private val answerArg = Opts.argument[String](metavar = "answer").orNone
  private val example = Opts.option[String]("example", short = "e", metavar = "example name", help = "Use the given example input instead of the official input.").orNone.map(_.getOrElse("official"))
  private val verbose = Opts.flag("verbose", short = "v", help = "Print full stack traces.").orFalse
  private val common = (Opts(year), day, part, example)

  private val run = Opts.subcommand("run", "Run the specified puzzle.")(common.mapN(RunPuzzle.apply))
  private val answer = Opts.subcommand("answer", "Specify the correct answer for the puzzle and clears all guesses. Copies from the clipboard by default.")((Opts(year), day, part, example, answerArg).mapN(Answer.apply))
  private val session = Opts.subcommand("session", "Copy the user's session cookie from the clipboard.")(Opts(Session()))
  private val database = Opts.subcommand("database", "Initialize the database at advent.db.")(Opts(InitDatabase()))

  private val showInput = Opts.subcommand("show", "Print the input to the console.")((Opts(year), day, example).mapN(ShowInput.apply))
  private val copyInput = Opts.subcommand("copy", "Copy the puzzle's input from the clipboard.")((Opts(year), day, example).mapN(Input.apply))
  private val input = Opts.subcommand("input", "Commands dealing with input.")(copyInput <+> showInput)

  private val scrape = Opts.subcommand("scrape", "Scrape examples from <code></code> tags in the puzzle description.")((Opts(year), day).mapN(Scrape.apply))
  private val list = Opts.subcommand("list", "List all examples in the database for this day.")((Opts(year), day).mapN(ListExamples.apply))
  private val examples = Opts.subcommand("examples", "Commands dealing with examples.")(scrape <+> list)

  private val all = List(run, input, answer, session, database, examples).combineAll

  private val opts = (verbose, all).tupled.map{(verbose, command) =>
    val result = command.run.as(ExitCode.Success)
    if verbose then
      result
    else
      result.handleErrorWith{
        case _: ClassNotFoundException => Console[IO].println("Puzzle not implemented").as(ExitCode.Error)
        case e => Console[IO].println(s"${e.getClass().getName()}: ${e.getMessage()}").as(ExitCode.Error)
      }
  }

  def main(args: Array[String]): Unit =
    CommandIOApp
      .run[IO]("run", s"$year Advent of Code Runner")(opts, args.toList)
      .unsafeRunSync()
