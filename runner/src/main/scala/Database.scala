package runner

import cats.effect.*
import cats.implicits.given
import doobie.*
import doobie.implicits.given

object Database:
  object InputNotFoundException extends Exception("Input not found in database.")

  private val xa = Transactor.fromDriverManager[IO](
    driver = "org.sqlite.JDBC",
    url = "jdbc:sqlite:../advent.db",
    logHandler = None
  )

  def init: IO[Unit] =
    val createSession = sql"CREATE TABLE IF NOT EXISTS session (session TEXT PRIMARY KEY)".update.run
    val createInput = sql"CREATE TABLE IF NOT EXISTS input (year INT, day INT, example TEXT, input TEXT, PRIMARY KEY (year, day, example))".update.run
    val createAnswers = sql"CREATE TABLE IF NOT EXISTS answers (year INT, day INT, part INT, example TEXT, answer TEXT, PRIMARY KEY (year, day, part, example))".update.run
    (createSession, createInput, createAnswers).tupled.transact(xa).void

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
    sql"INSERT INTO session (session) VALUES ($session) ON CONFLICT(session) DO UPDATE set session=$session".update.run.transact(xa).void

  def getSession: IO[String] =
    sql"SELECT session FROM session".query[String].unique.transact(xa)

  def setAnswer(year: Int, day: Int, part: Int, example: String, answer: String): IO[Unit] =
    sql"INSERT INTO answers (year, day, part, example, answer) VALUES ($year, $day, $part, $example, $answer) ON CONFLICT(year, day, part, example) DO UPDATE set answer=$answer".update.run.transact(xa).void

  def getAnswer(year: Int, day: Int, part: Int, example: String): IO[Option[String]] =
    sql"SELECT answer FROM answers WHERE year=$year AND day=$day AND part=$part AND example=$example".query[String].option.transact(xa)
