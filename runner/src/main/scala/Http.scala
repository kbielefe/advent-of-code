package runner

import cats.effect.*
import cats.effect.std.Console
import java.util.{Calendar, Date, TimeZone}
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder

object Http:
  object NotUnlockedException extends Exception("Puzzle is not yet unlocked.")

  private val client =
    EmberClientBuilder
      .default[IO]
      .build

  def downloadInput(year: Int, day: Int, session: String): IO[String] =
    val request = Request[IO](uri = Uri.unsafeFromString(s"https://adventofcode.com/$year/day/$day/input"))
      .withHeaders("User-Agent" -> "http4s https://github.com/kbielefe/advent-of-code/ by karl.bielefeldt@gmail.com")
      .addCookie("session", session)

    if unlocked(year, day) then
      Console[IO].println(s"Downloading input for $year $day") >>
      client.use{_.expect[String](request)}
    else
      IO.raiseError(NotUnlockedException)

  private def unlocked(year: Int, day: Int): Boolean =
    val calendar = Calendar.getInstance(TimeZone.getTimeZone("EST"))
    calendar.set(year, 11, day, 0, 0, 0)
    val unlockTime = calendar.getTimeInMillis()
    val currentTime = new Date().getTime()
    currentTime >= unlockTime
