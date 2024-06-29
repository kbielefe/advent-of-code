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
  private val quiet = Opts.flag("quiet", short = "q", help = "Don't play sounds after puzzle calculations.").orFalse
  private val help = Opts.flag("help", short = "h", help = "Show this help text.", Visibility.Partial).asHelp
  private val name = Opts.argument[String](metavar = "visualization name")
  private val common = (Opts(year), day, part, example)

  private val run = Opts.subcommand("run", "Run the specified puzzle.")((Opts(year), day, part, example, quiet).mapN(RunPuzzle.apply))
  private val visualization = Opts.subcommand("visualization", "Show a visualization of the puzzle.")((Opts(year), day, name, example).mapN(Visualization.apply))
  private val answer = Opts.subcommand("answer", "Specify the correct answer for the puzzle and clears all guesses. Copies from the clipboard by default.")((Opts(year), day, part, example, answerArg).mapN(Answer.apply))
  private val guess = Opts.subcommand("guess", "Show the guesses made so far")((Opts(year), day, part, example).mapN(Guesses.apply))
  private val session = Opts.subcommand("session", "Copy the user's session cookie from the clipboard.")(Opts(Session()))
  private val database = Opts.subcommand("database", "Initialize the database at advent.db.")(Opts(InitDatabase()))

  private val showInput = Opts.subcommand("show", "Print the input to the console.")((Opts(year), day, example).mapN(ShowInput.apply))
  private val copyInput = Opts.subcommand("copy", "Copy the puzzle's input from the clipboard.")((Opts(year), day, example).mapN(Input.apply))
  private val input = Opts.subcommand("input", "Commands dealing with input.")(copyInput <+> showInput)

  private val posExample = Opts.argument[String](metavar = "example name")
  private val showExamples = Opts.subcommand("show", "Show all examples in the database for this day.")((Opts(year), day).mapN(ShowExamples.apply))
  private val deleteExample = Opts.subcommand("delete", "Delete an example.")((Opts(year), day, posExample).mapN(DeleteExample.apply))
  private val examples = Opts.subcommand("example", "Commands dealing with examples.")(showExamples <+> deleteExample)

  private val all = List(help, run, input, answer, guess, session, database, examples, visualization).combineAll

  private val opts = (verbose, all).tupled.map{(verbose, command) =>
    val result = command.run.as(ExitCode.Success)
    if verbose then
      result
    else
      result.handleErrorWith:
        case _: ClassNotFoundException => Console[IO].println("Puzzle not implemented").as(ExitCode.Error)
        case e => Console[IO].println(s"${e.getClass().getName()}: ${e.getMessage()}").as(ExitCode.Error)
  }

  def main(args: Array[String]): Unit =
    CommandIOApp
      .run[IO]("run", s"$year Advent of Code Runner", helpFlag=false)(opts, args.toList)
      .unsafeRunSync()
