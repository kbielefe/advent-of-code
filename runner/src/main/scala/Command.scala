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
    val inputIO = example match
      case Some(example) => Database.exampleInput(year, day, example)
      case None          => Database.officialInput(year, day)
    inputIO.map { input => if part == 1 then getDay.normalizedPart1(input) else getDay.normalizedPart2(input) }
      .flatTap(Console[IO].println)

  def getDay: NormalizedDay =
    Class.forName(s"Day$day$$")
      .getField("MODULE$")
      .get(null)
      .asInstanceOf[NormalizedDay]

case class RunPuzzle(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] = answer.flatMap(checkAnswer)

  def checkAnswer(answer: String): IO[Unit] = IO.unit

case class Correct(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)
case class Incorrect(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)
case class High(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)
case class Low(year: Int, day: Int, part: Int, example: Option[String]) extends AnswerCommand(year, day, part, example)

case class Input(year: Int, day: Int, example: Option[String]) extends Command
case class Answer(year: Int, day: Int, part: Int, example: Option[String], answer: Option[String]) extends Command
case class Session() extends Command:
  override def run: IO[Unit] =
    val session = paste
    Console[IO].println(s"Setting session cookie to $session") >>
    Database.setSession(session)

case class InitDatabase() extends Command:
  override def run: IO[Unit] = Database.init
