package runner

import cats.effect.*
import cats.effect.std.Console
import doobie.*
import doobie.implicits.given
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder

object Database:
  private val xa = Transactor.fromDriverManager[IO](
    driver = "org.sqlite.JDBC",
    url = "jdbc:sqlite:../advent.db",
    logHandler = None
  )

  def init: IO[Unit] =
    sql"CREATE TABLE IF NOT EXISTS session (session TEXT)".update.run.transact(xa) >>
    sql"CREATE TABLE IF NOT EXISTS exampleinput (year INT, day Int, example TEXT, input TEXT)".update.run.transact(xa) >>
    sql"CREATE TABLE IF NOT EXISTS input (year INT, day Int, input TEXT)".update.run.transact(xa).void

  def officialInput(year: Int, day: Int): IO[String] =
    sql"SELECT input FROM input WHERE year = $year AND day = $day".query[String].option.transact(xa).flatMap{
      case Some(input) => IO.pure(input)
      case None        => getSession.flatMap(downloadInput(year, day)).flatMap(insertOfficialInput(year, day))
    }

  def downloadInput(year: Int, day: Int)(session: String): IO[String] =
    val request = Request[IO](uri = Uri.unsafeFromString(s"https://adventofcode.com/$year/day/$day/input"))
      .withHeaders("User-Agent" -> "http4s https://github.com/kbielefe/advent-of-code/ by karl.bielefeldt@gmail.com")
      .addCookie("session", session)
    Console[IO].println(s"Downloading input for $year $day") >>
    EmberClientBuilder
      .default[IO]
      .build
      .use{_.expect[String](request)}

  def insertOfficialInput(year: Int, day: Int)(input: String): IO[String] =
    sql"INSERT INTO input (year, day, input) VALUES ($year, $day, $input)".update.run.transact(xa) >> IO.pure(input)

  def exampleInput(year: Int, day: Int, example: String): IO[String] =
    sql"SELECT input FROM exampleinput WHERE year = $year AND day = $day AND example = $example".query[String].unique.transact(xa)

  def setSession(session: String): IO[Unit] =
    sql"DELETE FROM session".update.run.transact(xa) >>
    sql"INSERT INTO session (session) VALUES ($session)".update.run.transact(xa).void

  def getSession: IO[String] =
    sql"SELECT session FROM session".query[String].unique.transact(xa)
