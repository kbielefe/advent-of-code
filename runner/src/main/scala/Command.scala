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
  def answer: IO[String] =
    Database
      .getInput(year, day, example)
      .flatMap(input => if part == 1 then IO.blocking(getDay.normalizedPart1(input)) else IO.blocking(getDay.normalizedPart2(input)))
      .flatTap(Console[IO].println)

  def getDay: NormalizedDay =
    Class.forName(s"day$day.Puzzle$$")
      .getField("MODULE$")
      .get(null)
      .asInstanceOf[NormalizedDay]

case class RunPuzzle(year: Int, day: Int, part: Int, example: String) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] = answer.flatMap(checkAnswer)

  def checkAnswer(answer: String): IO[Unit] =
    (Database.getAnswer(year, day, part, example), Database.getGuesses(year, day, part, example)).tupled.flatMap {
      case (Some(correct), _) if answer == correct => IO.pure("Correct")
      case (Some(correct), _)                      => incorrect(answer, correct)
      case (None, guesses)                         => unknown(answer, guesses)
    }.flatMap(Console[IO].println)

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

  def unknown(answer: String, guesses: List[(String, String)]): IO[String] =
    val highOrLow = toBigInt(answer).flatMap { answer =>
      val numericGuesses = guesses.flatMap((status, guess) => toBigInt(guess).map(status -> _))
      val lows = numericGuesses.filter(_._1 == "low").map(_._2)
      val highs = numericGuesses.filter(_._1 == "high").map(_._2)
      (lows.isEmpty, highs.isEmpty) match
        case (false, true) if answer <= lows.max  => Some("low")
        case (true, false) if answer >= highs.max => Some("high")
        case _ => None
    }
    val result = highOrLow match
      case Some("low")  => "Too low"
      case Some("high") => "Too high"
      case _            => "Unknown"
    val addGuess = highOrLow.map(status => Database.addGuess(year, day, part, example, status, answer)).getOrElse(IO.unit)
    addGuess *> IO.pure(result)

  def toBigInt(string: String): Option[BigInt] =
    Try(BigInt(string)).toOption

case class Correct(year: Int, day: Int, part: Int, example: String) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] =
    answer.flatMap(Database.setAnswer(year, day, part, example, _))

case class Incorrect(year: Int, day: Int, part: Int, example: String) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] =
    answer.flatMap(Database.addGuess(year, day, part, example, "incorrect", _))

case class High(year: Int, day: Int, part: Int, example: String) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] =
    answer.flatMap(Database.addGuess(year, day, part, example, "high", _))

case class Low(year: Int, day: Int, part: Int, example: String) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] =
    answer.flatMap(Database.addGuess(year, day, part, example, "low", _))

case class Input(year: Int, day: Int, example: String) extends Command:
  override def run: IO[Unit] =
    val input = paste
    Console[IO].println(s"Setting input to:\n$input") >>
    Database.setInput(year, day, example, input)

case class Answer(year: Int, day: Int, part: Int, example: String, answer: Option[String]) extends Command:
  override def run: IO[Unit] =
    val result = answer.getOrElse(paste)
    Console[IO].println(s"Setting answer for day $day part $part example $example to $result") >>
    Database.setAnswer(year, day, part, example, result)

case class Session() extends Command:
  override def run: IO[Unit] =
    val session = paste
    Console[IO].println(s"Setting session cookie to $session") >>
    Database.setSession(session)

case class InitDatabase() extends Command:
  override def run: IO[Unit] = Database.init
