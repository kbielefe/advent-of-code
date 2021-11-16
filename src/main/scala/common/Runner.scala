import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.util.{Calendar, Date, TimeZone}
import puzzleparse.{*, given}
import sttp.client3.*

object Runner:
  @main def main(year: Int, day: Int, part: Int): Unit =
    if !unlocked(year, day) then
      println("Puzzle input not yet unlocked")
      return
    val input = getOrDownloadInput(year, day)
    val run = runDay(input, part)
    (year, day) match
      case (2020, 1) => run(advent2020.Day1.part1, advent2020.Day1.part2)
      case (2020, 2) => run(advent2020.Day2.part1, advent2020.Day2.part2)
      case _         => println("Puzzle solution not found.")

  private class runDay(input: String, part: Int):
    def apply[A: Read, B: Show, C: Read, D: Show](part1: A => B, part2: C => D): Unit =
      val start = System.currentTimeMillis()
      val output = if part == 1 then run(part1) else run(part2)
      val end = System.currentTimeMillis()
      copyToClipboard(output)
      println(s"(${end - start} ms)\n$output")

    private def run[A, B](f: A => B)(using read: Read[A], show: Show[B]): String =
      show.show(f(read.read(input)))

    private def copyToClipboard(text: String): Unit =
      Toolkit
        .getDefaultToolkit()
        .getSystemClipboard()
        .setContents(new StringSelection(text), null)
  end runDay

  private def getOrDownloadInput(year: Int, day: Int): String =
    val filename = os.pwd / "input" / s"$year" / s"$day.txt"
    if os.exists(filename) then
      os.read(filename)
    else
      println("Downloading new input")
      val session = os.read(os.pwd / "input" / "session").trim
      val backend = HttpURLConnectionBackend()
      val response = basicRequest
        .cookie("session", session)
        .get(uri"https://adventofcode.com/$year/day/$day/input")
        .send(backend)
      val text = response.body match
        case Left(e) => throw new Exception(s"Error downloading input: $e")
        case Right(text) => text
      os.write(filename, text)
      text

  private def unlocked(year: Int, day: Int): Boolean =
    val calendar = Calendar.getInstance(TimeZone.getTimeZone("EST"))
    calendar.set(year, 11, day, 0, 0, 0)
    val unlockTime = calendar.getTimeInMillis()
    val currentTime = new Date().getTime()
    currentTime >= unlockTime

end Runner
