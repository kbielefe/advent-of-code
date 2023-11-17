package runner

import cats.effect.*
import cats.effect.std.Console
import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}

sealed trait Command:
  def run: IO[Unit] = ???

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

sealed trait AnswerCommand(year: Int, day: Int, part: Int, example: Option[String]) extends Command:
  def answer: IO[String] =
    Database
      .getInput(year, day, example.getOrElse("official"))
      .map(input => if part == 1 then getDay.normalizedPart1(input) else getDay.normalizedPart2(input))
      .flatTap(Console[IO].println)

  def getDay: NormalizedDay =
    Class.forName(s"Day$day$$")
      .getField("MODULE$")
      .get(null)
      .asInstanceOf[NormalizedDay]

case class RunPuzzle(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] = answer.flatMap(checkAnswer)

  def checkAnswer(answer: String): IO[Unit] =
    Database.getAnswer(year, day, part, example.getOrElse("official")).map {
      case Some(correct) if answer == correct => "Correct"
      case Some(correct)                      => s"Incorrect, should be $correct"
      case None                               => "Unknown"
    }.flatMap(Console[IO].println)

case class Correct(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)
case class Incorrect(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)
case class High(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)
case class Low(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)

case class Input(year: Int, day: Int, example: Option[String]) extends Command:
  override def run: IO[Unit] =
    val input = paste
    Console[IO].println(s"Setting input to:\n$input") >>
    Database.setInput(year, day, example.getOrElse("official"), input)

case class Answer(year: Int, day: Int, part: Int, example: Option[String], answer: Option[String]) extends Command:
  override def run: IO[Unit] =
    val result = answer.getOrElse(paste)
    Console[IO].println(s"Setting answer for day $day part $part example $example to $result") >>
    Database.setAnswer(year, day, part, example.getOrElse("official"), result)

case class Session() extends Command:
  override def run: IO[Unit] =
    val session = paste
    Console[IO].println(s"Setting session cookie to $session") >>
    Database.setSession(session)

case class InitDatabase() extends Command:
  override def run: IO[Unit] = Database.init
