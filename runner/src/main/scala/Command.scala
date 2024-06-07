package runner

import cats.effect.*
import cats.effect.std.Console
import cats.implicits.given
import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}
import scala.util.Try

sealed trait Command:
  def run: IO[Unit]

  def copy(string: String): Unit =
    Toolkit
      .getDefaultToolkit()
      .getSystemClipboard()
      .setContents(new StringSelection(string), null)

  def paste: String =
    Toolkit
      .getDefaultToolkit()
      .getSystemClipboard()
      .getContents(null)
      .getTransferData(DataFlavor.stringFlavor)
      .asInstanceOf[String]

sealed trait AnswerCommand(year: Int, day: Int, part: Int, example: String) extends Command:
  def answer: IO[String] = for
    input  <- Database.getInput(year, day, example)
    _      <- Database.scrapeMissingExamples(year, day)
    answer <- if part == 1 then getDay.normalizedPart1(input.stripTrailing()) else getDay.normalizedPart2(input.stripTrailing())
    _      <- Console[IO].println(answer)
  yield answer

  def getDay: NormalizedDay =
    Class
      .forName(s"day$day.Puzzle$$")
      .getField("MODULE$")
      .get(null)
      .asInstanceOf[NormalizedDay]

case class Visualization(year: Int, day: Int, name: String, example: String) extends Command:
  override def run: IO[Unit] = for
    input  <- Database.getInput(year, day, example)
    _      <- runVisualization(input)
  yield ()

  def runVisualization(input: String): IO[Unit] =
    val c = Class.forName(s"day$day.Puzzle$$")
    val obj = c.getField("MODULE$").get(null).asInstanceOf[NormalizedDay]
    c.getMethods().find(_.getName() == name) match
      case Some(method) =>
        val arg = obj.read(input)
        if method.getReturnType().getName() == "cats.effect.IO" then
          method.invoke(obj, arg).asInstanceOf[IO[Unit]]
        else
          IO.interruptible(method.invoke(obj, arg))
      case None =>
        IO.raiseError(new Exception(s"Visualization $name not found"))

case class RunPuzzle(year: Int, day: Int, part: Int, example: String) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] = answer.flatMap(checkAnswer)

  def checkAnswer(answer: String): IO[Unit] =
    (Database.getAnswer(year, day, part, example), Database.getGuesses(year, day, part, example)).tupled.flatMap {
      case (Some(correct), _) if answer == correct => Console[IO].println("Correct")
      case (Some(correct), _)                      => incorrect(answer, correct).flatMap(Console[IO].println)
      case (None, guesses)                         => unknown(answer, guesses)
    }

  def incorrect(answer: String, correct: String): IO[String] =
    val highOrLow = for
      answerInt  <- toBigInt(answer)
      correctInt <- toBigInt(correct)
    yield if answerInt < correctInt then "low" else "high"
    val status = highOrLow.getOrElse("incorrect")
    val result = status match
      case "incorrect" => s"Incorrect, should be $correct"
      case "high"      => s"Too high, should be $correct"
      case "low"       => s"Too low, should be $correct"
    Database.addGuess(year, day, part, example, status, answer) *> IO.pure(result)

  def unknown(answer: String, guesses: List[(String, String)]): IO[Unit] =
    val highOrLow = toBigInt(answer).flatMap { answer =>
      val numericGuesses = guesses.flatMap((status, guess) => toBigInt(guess).map(status -> _))
      val lows = numericGuesses.filter(_._1 == "low").map(_._2)
      val highs = numericGuesses.filter(_._1 == "high").map(_._2)
      (lows.isEmpty, highs.isEmpty) match
        case (false, _) if answer <= lows.max  => Some(s"Too low, should be higher than ${lows.max}")
        case (_, false) if answer >= highs.min => Some(s"Too high, should be lower than ${highs.min}")
        case _ => None
    }
    highOrLow match
      case Some(message) => Console[IO].println(message)
      case None if guesses.contains(("incorrect", answer)) => Console[IO].println("Incorrect (already guessed)")
      case _ =>
        IO(copy(answer)) >>
        Console[IO].print("[c]orrect, [i]ncorrect, [h]igh, or [l]ow? ") >>
        Console[IO].readLine.flatMap{
          case "c" => Database.setAnswer(year, day, part, example, answer)
          case "i" => Database.addGuess(year, day, part, example, "incorrect", answer)
          case "h" => Database.addGuess(year, day, part, example, "high", answer)
          case "l" => Database.addGuess(year, day, part, example, "low", answer)
          case _   => IO.unit
        }

  def toBigInt(string: String): Option[BigInt] =
    Try(BigInt(string)).toOption

case class Input(year: Int, day: Int, example: String) extends Command:
  override def run: IO[Unit] =
    val input = paste
    Console[IO].println(s"Setting input to:\n$input") >>
    Database.setInput(year, day, example, input)

case class ShowInput(year: Int, day: Int, example: String) extends Command:
  override def run: IO[Unit] =
    Database.getInput(year, day, example).flatMap(Console[IO].println)

case class Answer(year: Int, day: Int, part: Int, example: String, answer: Option[String]) extends Command:
  override def run: IO[Unit] =
    val result = answer.getOrElse(paste)
    Console[IO].println(s"Setting answer for day $day part $part example $example to $result") >>
    Database.setAnswer(year, day, part, example, result)

case class Guesses(year: Int, day: Int, part: Int, example: String) extends Command:
  override def run: IO[Unit] =
    Console[IO].println(s"Guesses for day $day part $part example $example:") >>
    Database.getGuesses(year, day, part, example).flatMap{guesses =>
      guesses.sortBy(_._2).traverse((status, guess) => Console[IO].println(f"$status%-5s $guess"))
    }.void

case class Session() extends Command:
  override def run: IO[Unit] =
    val session = paste
    Console[IO].println(s"Setting session cookie to $session") >>
    Database.setSession(session)

case class InitDatabase() extends Command:
  override def run: IO[Unit] = Database.init

case class Scrape(year: Int, day: Int) extends Command:
  override def run: IO[Unit] =
    Database.scrapeExamples(year, day)

case class ShowExamples(year: Int, day: Int) extends Command:
  override def run: IO[Unit] =
    Database.showExamples(year, day)

case class DeleteExample(year: Int, day: Int, example: String) extends Command:
  override def run: IO[Unit] =
    Database.deleteExample(year, day, example)
