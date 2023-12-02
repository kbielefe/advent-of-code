package runner

import cats.effect.*
import cats.effect.std.Console
import cats.implicits.given
import doobie.*
import doobie.implicits.given

object Database:
  object InputNotFoundException extends Exception("Input not found in database.")

  private val xa = Transactor.fromDriverManager[IO](
    driver = "org.sqlite.JDBC",
    url = "jdbc:sqlite:advent.db",
    logHandler = None
  )

  def init: IO[Unit] =
    val createSession = sql"CREATE TABLE IF NOT EXISTS session (session TEXT PRIMARY KEY)".update.run
    val createInput   = sql"CREATE TABLE IF NOT EXISTS input (year INT, day INT, example TEXT, input TEXT, PRIMARY KEY (year, day, example))".update.run
    val createAnswers = sql"CREATE TABLE IF NOT EXISTS answers (year INT, day INT, part INT, example TEXT, answer TEXT, PRIMARY KEY (year, day, part, example))".update.run
    val createGuesses = sql"CREATE TABLE IF NOT EXISTS guesses (year INT, day INT, part INT, example TEXT, status TEXT, guess TEXT)".update.run
    (createSession, createInput, createAnswers, createGuesses).tupled.transact(xa).void

  def getInput(year: Int, day: Int, example: String): IO[String] =
    sql"SELECT input FROM input WHERE year = $year AND day = $day AND example = $example".query[String].option.transact(xa).flatMap{
      case Some(input)                   => IO.pure(input)
      case None if example == "official" => downloadInput(year, day, example)
      case _                             => IO.raiseError(InputNotFoundException)
    }

  def downloadInput(year: Int, day: Int, example: String): IO[String] = for
    session <- getSession
    input   <- Http.downloadInput(year, day, session)
    _       <- setInput(year, day, example, input)
  yield input

  def setInput(year: Int, day: Int, example: String, input: String): IO[Unit] =
    sql"INSERT INTO input (year, day, example, input) VALUES ($year, $day, $example, $input) ON CONFLICT(year, day, example) DO UPDATE set input=$input".update.run.transact(xa).void

  def setSession(session: String): IO[Unit] =
    val truncate = sql"DELETE FROM session".update.run
    val insert = sql"INSERT INTO session (session) VALUES ($session)".update.run
    (truncate >> insert).transact(xa).void

  def getSession: IO[String] =
    sql"SELECT session FROM session".query[String].unique.transact(xa)

  def setAnswer(year: Int, day: Int, part: Int, example: String, answer: String): IO[Unit] =
    val deleteGuesses = sql"DELETE FROM guesses WHERE year=$year AND day=$day AND part=$part AND example=$example".update.run
    val insertAnswer = sql"INSERT INTO answers (year, day, part, example, answer) VALUES ($year, $day, $part, $example, $answer) ON CONFLICT(year, day, part, example) DO UPDATE set answer=$answer".update.run
    (deleteGuesses, insertAnswer).tupled.transact(xa).void

  def getAnswer(year: Int, day: Int, part: Int, example: String): IO[Option[String]] =
    sql"SELECT answer FROM answers WHERE year=$year AND day=$day AND part=$part AND example=$example".query[String].option.transact(xa)

  def addGuess(year: Int, day: Int, part: Int, example: String, status: String, guess: String): IO[Unit] =
    sql"INSERT INTO guesses (year, day, part, example, status, guess) VALUES ($year, $day, $part, $example, $status, $guess)".update.run.transact(xa).void

  def getGuesses(year: Int, day: Int, part: Int, example: String): IO[List[(String, String)]] =
    sql"SELECT status, guess FROM guesses WHERE year=$year AND day=$day AND part=$part AND example=$example".query[(String, String)].to[List].transact(xa)

  def scrapeMissingExamples(year: Int, day: Int): IO[Unit] = for
    count   <- sql"SELECT COUNT(input) FROM input WHERE year = $year AND day = $day AND example != 'official'".query[Int].unique.transact(xa)
    _       <- IO.unlessA(count > 0)(scrapeExamples(year, day))
  yield ()

  def scrapeExamples(year: Int, day: Int): IO[Unit] =
    getSession.flatMap{session =>
      Http.scrapeExamples(year, day, session)
        .zipWithIndex
        .foreach(saveExample(year, day))
        .compile
        .drain
    }

  def saveExample(year: Int, day: Int)(example: (String, Long)): IO[Unit] =
    val (input, index) = example
    val exampleString = (index + 1).toString
    Console[IO].println("") >>
    Console[IO].println(s"Saving example $exampleString to database:\n$input") >>
    setInput(year, day, exampleString, input)

  def listExamples(year: Int, day: Int): IO[Unit] =
    sql"SELECT input, example FROM input WHERE year = $year AND day = $day AND example != 'official'".query[(String, String)].to[List].transact(xa).flatMap{examples =>
      examples.traverse{case (input, example) => Console[IO].println(s"\nExample $example:\n$input")}.void
    }
