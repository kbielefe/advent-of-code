package runner

import cats.effect.*
import cats.effect.kernel.Resource
import cats.effect.std.Console
import cats.implicits.given
import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.io.BufferedInputStream
import javax.sound.sampled.*
import scala.concurrent.duration.*
import scala.util.Try
import visualizations.NormalizedVisualization

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

  /** clip.start() runs in the background, so we need to run the next IO and
   *  not close the program until the clip finishes.
   */
  def beep[A](name: String, quiet: Boolean)(next: IO[A]): IO[A] =
    val clipResource = for
      clip        <- Resource.fromAutoCloseable(IO(AudioSystem.getClip()))
      stream      <- Resource.fromAutoCloseable(IO(getClass().getResourceAsStream(s"/$name.wav")))
      buffered    <- Resource.fromAutoCloseable(IO(new BufferedInputStream(stream)))
      audioStream <- Resource.fromAutoCloseable(IO(AudioSystem.getAudioInputStream(buffered)))
      _           <- Resource.eval(IO(clip.open(audioStream)))
      _           <- Resource.eval(IO(clip.start()))
    yield clip

    val loud = clipResource.use{clip =>
      for
        fiber  <- IO.sleep(clip.getMicrosecondLength().microseconds).start
        result <- next
        _ <- fiber.joinWithUnit
      yield result
    }

    if quiet then next else loud

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

case class ListVisualizations(year: Int, day: Option[Int]) extends Command:
  override def run: IO[Unit] =
    day match
      case Some(day) => listForDay(day)
      case None => (1 to 25).toList.map(listForDay).sequence.void

  private def listForDay(day: Int): IO[Unit] =
    val list = Try{Class.forName(s"day$day.Puzzle$$")}.map: c =>
      val obj = c.getField("MODULE$").get(null).asInstanceOf[NormalizedDay]
      val visualizations =
        c.getMethods()
          .toList
          .filter(_.getReturnType().getInterfaces().map(_.getName()).contains("visualizations.Visualization"))
          .map(_.invoke(obj).asInstanceOf[NormalizedVisualization])
      visualizations.map(vis => Console[IO].println(s"Day $day ${vis.name} ${vis.description}")).sequence.void
    list.getOrElse(IO.unit)

case class Visualization(year: Int, day: Int, name: String, example: String) extends Command:
  override def run: IO[Unit] = for
    input  <- Database.getInput(year, day, example)
    _      <- runVisualization(input)
  yield ()

  def runVisualization(input: String): IO[Unit] =
    val c = Class.forName(s"day$day.Puzzle$$")
    val obj = c.getField("MODULE$").get(null).asInstanceOf[NormalizedDay]
    val visualizations =
      c.getMethods()
        .toList
        .filter(_.getReturnType().getInterfaces().map(_.getName()).contains("visualizations.Visualization"))
        .map(_.invoke(obj).asInstanceOf[NormalizedVisualization])
    visualizations.find(_.name == name) match
      case Some(vis) => vis.show(input)
      case None      => IO.raiseError(new Exception(s"Visualization $name not found"))

case class RunPuzzle(year: Int, day: Int, part: Int, example: String, quiet: Boolean) extends AnswerCommand(year, day, part, example):
  override def run: IO[Unit] = for
    answer   <- answer
    dbAnswer <- Database.getAnswer(year, day, part, example)
    guesses  <- Database.getGuesses(year, day, part, example)
    result    = checkAnswer(answer, dbAnswer, guesses)
    _        <- result.report(answer)
  yield ()

  def checkAnswer(answer: String, dbAnswer: Option[String], guesses: List[(String, String)]): Result = (dbAnswer, guesses) match
    case (Some(correct), _) if answer == correct => Correct
    case (Some(correct), _)                      => incorrect(answer, correct)
    case (None, guesses)                         => unknown(answer, guesses)

  def incorrect(answer: String, correct: String): Result =
    val low = for
      answerInt  <- toBigInt(answer)
      correctInt <- toBigInt(correct)
    yield (answerInt < correctInt, correctInt)

    low match
      case Some(true  -> correct) => Low(correct, true)
      case Some(false -> correct) => High(correct, true)
      case _                      => Incorrect(Some(correct))

  def unknown(answer: String, guesses: List[(String, String)]): Result =
    val numericGuesses = guesses.flatMap((status, guess) => toBigInt(guess).map(status -> _))
    val lows  = numericGuesses.filter(_._1 ==  "low").map(_._2)
    val highs = numericGuesses.filter(_._1 == "high").map(_._2)

    toBigInt(answer) match
      case Some(answer) if !lows.isEmpty  && answer <= lows.max  => Low(lows.max, false)
      case Some(answer) if !highs.isEmpty && answer >= highs.min => High(highs.min, false)
      case _ if guesses.contains(("incorrect", answer))          => Incorrect(None)
      case _                                                     => Unknown

  def toBigInt(string: String): Option[BigInt] =
    Try(BigInt(string)).toOption

  sealed trait Result:
    def report(answer: String): IO[Unit]

  case class High(limit: BigInt, knownCorrect: Boolean) extends Result:
    def report(answer: String): IO[Unit] =
      beep("high", quiet)(
        Database.addGuess(year, day, part, example, "high", answer) >>
        Console[IO].println(s"Too high, should be ${if knownCorrect then limit.toString else s"lower than $limit"}")
      )

  case class Low(limit: BigInt, knownCorrect: Boolean) extends Result:
    def report(answer: String): IO[Unit] =
      beep("low", quiet)(
        Database.addGuess(year, day, part, example, "low", answer) >>
        Console[IO].println(s"Too low, should be ${if knownCorrect then limit.toString else s"higher than $limit"}")
      )

  case class Incorrect(knownCorrect: Option[String]) extends Result:
    def report(answer: String): IO[Unit] =
      beep("incorrect", quiet)(
        Database.addGuess(year, day, part, example, "incorrect", answer) >>
        Console[IO].println(s"Incorrect, ${knownCorrect.fold("already guessed")(correct => s"should be $correct")}")
      )

  case object Correct extends Result:
    def report(answer: String): IO[Unit] =
      beep("correct", quiet)(
        Console[IO].println("Correct")
      )
  case object Unknown extends Result:
    def report(answer: String): IO[Unit] =
      beep("unknown", quiet)(
        IO(copy(answer)) >>
        Console[IO].print("[c]orrect, [i]ncorrect, [h]igh, or [l]ow? ") >>
        Console[IO].readLine.flatMap{
          case "c" => Database.setAnswer(year, day, part, example, answer)
          case "i" => Database.addGuess(year, day, part, example, "incorrect", answer)
          case "h" => Database.addGuess(year, day, part, example, "high", answer)
          case "l" => Database.addGuess(year, day, part, example, "low", answer)
          case _   => IO.unit
        }
      )

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
