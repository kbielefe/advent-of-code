package runner

import cats.effect.*
import cats.effect.std.Console
import fs2.*
import fs2.text.utf8.decode
import java.util.{Calendar, Date, TimeZone}
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder
import org.jsoup.Jsoup
import scala.jdk.CollectionConverters.*

object Http:
  object NotUnlockedException extends Exception("Puzzle is not yet unlocked.")

  private val client =
    EmberClientBuilder
      .default[IO]
      .build

  private def request(session: String, uriString: String): Request[IO] =
    Request[IO](uri = Uri.unsafeFromString(uriString))
      .withHeaders("User-Agent" -> "http4s https://github.com/kbielefe/advent-of-code/ by karl.bielefeldt@gmail.com")
      .addCookie("session", session)

  def downloadInput(year: Int, day: Int, session: String): IO[String] =
    if unlocked(year, day) then
      Console[IO].println(s"Downloading input for $year $day") >>
      client.use{_.expect[String](request(session, s"https://adventofcode.com/$year/day/$day/input"))}
    else
      IO.raiseError(NotUnlockedException)

  def scrapeExamples(year: Int, day: Int, session: String): Stream[IO, String] =
    if unlocked(year, day) then
      for
        client   <- Stream.resource(client)
        response <- client.stream(request(session, s"https://adventofcode.com/$year/day/$day"))
        html     <- Stream.eval(response.body.through(decode).compile.string)
        examples <- findExamples(html)
      yield examples
    else
      Stream.raiseError[IO](NotUnlockedException)

  private def unlocked(year: Int, day: Int): Boolean =
    val calendar = Calendar.getInstance(TimeZone.getTimeZone("EST"))
    calendar.set(year, 11, day, 0, 0, 0)
    val unlockTime = calendar.getTimeInMillis()
    val currentTime = new Date().getTime()
    currentTime >= unlockTime

  private def findExamples(html: String): Stream[IO, String] =
    val examples = Jsoup
      .parse(html)
      .getElementsByTag("pre").asScala
      .flatMap(_.getElementsByTag("code").asScala)
      .map(_.text())
    Stream.emits(examples)
