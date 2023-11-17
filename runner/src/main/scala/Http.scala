package runner

import cats.effect.*
import cats.effect.std.Console
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder

object Http:
  private val client =
    EmberClientBuilder
      .default[IO]
      .build

  def downloadInput(year: Int, day: Int, session: String): IO[String] =
    val request = Request[IO](uri = Uri.unsafeFromString(s"https://adventofcode.com/$year/day/$day/input"))
      .withHeaders("User-Agent" -> "http4s https://github.com/kbielefe/advent-of-code/ by karl.bielefeldt@gmail.com")
      .addCookie("session", session)

    Console[IO].println(s"Downloading input for $year $day") >>
    client.use{_.expect[String](request)}
