import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.util.{Calendar, Date, TimeZone}
import puzzleparse.{*, given}
import scala.util.Try
import sttp.client3.*

object Runner:
  @main def advent(year: Int, day: Int, part: Int, optional: String*): Unit =
    if !unlocked(year, day) then
      println("Puzzle input not yet unlocked")
      return
    val input = if optional.headOption.map(_ == "example").getOrElse(false) then
      val example = optional(1).toInt
      if optional.length > 2 then exampleAnswer(year, day, part, example, optional(2))
      getExample(year, day, example)
    else
      getOrDownloadInput(year, day)
    val run = runDay(input, year, day, part, optional)
    (year, day) match
      case (2020, 1) => run(advent2020.Day1.part1, advent2020.Day1.part2)
      case (2020, 2) => run(advent2020.Day2.part1, advent2020.Day2.part2)
      case (2020, 3) => run(advent2020.Day3.part1, advent2020.Day3.part2)
      case (2020, 4) => run(advent2020.Day4.part1, advent2020.Day4.part2)
      case (2020, 5) => run(advent2020.Day5.part1, advent2020.Day5.part2)
      case (2020, 6) => run(advent2020.Day6.part1, advent2020.Day6.part2)
      case (2020, 7) => run(advent2020.Day7.part1, advent2020.Day7.part2)
      case (2020, 8) => run(advent2020.Day8.part1, advent2020.Day8.part2)
      case (2020, 9) => run(advent2020.Day9.part1, advent2020.Day9.part2)
      case (2020, 10) => run(advent2020.Day10.part1, advent2020.Day10.part2)
      case (2020, 11) => run(advent2020.Day11.part1, advent2020.Day11.part2)
      case (2020, 12) => run(advent2020.Day12.part1, advent2020.Day12.part2)
      case (2020, 13) => run(advent2020.Day13.part1, advent2020.Day13.part2)
      case (2020, 14) => run(advent2020.Day14.part1, advent2020.Day14.part2)
      case (2021, 1) => run(advent2021.Day1.part1, advent2021.Day1.part2)
      case (2021, 2) => run(advent2021.Day2.part1, advent2021.Day2.part2)
      case (2021, 3) => run(advent2021.Day3.part1, advent2021.Day3.part2)
      case (2021, 4) => run(advent2021.Day4.part1, advent2021.Day4.part2)
      case _         => println("Puzzle solution not found.")

  private class runDay(input: String, year: Int, day: Int, part: Int, optional: Seq[String]):
    def apply[A: Read, B: Show, C: Read, D: Show](part1: A => B, part2: C => D): Unit =
      val start = System.currentTimeMillis()
      val output = if part == 1 then run(part1) else run(part2)
      val end = System.currentTimeMillis()
      println(s"(${end - start} ms)\n$output")
      copyToClipboard(output)
      runCommand(year, day, part, optional, output)

    private def run[A, B](f: A => B)(using read: Read[A], show: Show[B]): String =
      show.show(f(read.read(input)))

    private def copyToClipboard(text: String): Unit =
      Toolkit
        .getDefaultToolkit()
        .getSystemClipboard()
        .setContents(new StringSelection(text), null)
  end runDay

  private def runCommand(year: Int, day: Int, part: Int, optional: Seq[String], output: String): Unit =
    val command = optional.headOption.getOrElse("")
    if command.startsWith("correct") then
      markCorrect(year, day, part, output)
    if command.startsWith("incorrect") then
      markIncorrect(year, day, part, output)
    else if command.startsWith("high") then
      markHigh(year, day, part, output)
    else if command.startsWith("low") then
      markLow(year, day, part, output)
    else
      checkAnswer(year, day, part, output, optional)

  private def getAnswer(year: Int, day: Int, part: Int, kind: String): Option[String] =
    val filename = os.pwd / "input" / s"$year" / s"${day}_answers.txt"
    os.read.lines(filename)
      .find(_.startsWith(s"part $part $kind"))
      .map(_.split(" ").last)

  private def getIncorrect(year: Int, day: Int, part: Int): IndexedSeq[String] =
    val filename = os.pwd / "input" / s"$year" / s"${day}_answers.txt"
    os.read.lines(filename)
      .filter(_.startsWith(s"part $part incorrect"))
      .map(_.split(" ").last)

  private def exampleAnswer(year: Int, day: Int, part: Int, example: Int, answer: String): Unit =
    modifyAnswers(year, day, part, s"example $example", s"example $example $answer")

  private def markCorrect(year: Int, day: Int, part: Int, answer: String): Unit =
    modifyAnswers(year, day, part, s"correct|incorrect|low|high", s"correct $answer")

  private def markIncorrect(year: Int, day: Int, part: Int, answer: String): Unit =
    modifyAnswers(year, day, part, s"incorrect $answer", s"incorrect $answer")

  private def markHigh(year: Int, day: Int, part: Int, answer: String): Unit =
    val correct = getAnswer(year, day, part, "correct")
    val high = getAnswer(year, day, part, "high")
    if !correct.isDefined && high.map(num => BigInt(num) < BigInt(answer)).getOrElse(true) then
      modifyAnswers(year, day, part, s"high", s"high $answer")

  private def markLow(year: Int, day: Int, part: Int, answer: String): Unit =
    val correct = getAnswer(year, day, part, "correct")
    val low = getAnswer(year, day, part, "low")
    if !correct.isDefined && low.map(num => BigInt(num) > BigInt(answer)).getOrElse(true) then
      modifyAnswers(year, day, part, s"low", s"low $answer")

  private def checkAnswer(year: Int, day: Int, part: Int, answer: String, optional: Seq[String]): Unit =
    val example = optional.headOption.map(_ == "example").getOrElse(false)
    val correctString = if example then s"example ${optional(1).toInt}" else "correct"
    val correct = getAnswer(year, day, part, correctString)
    val high = getAnswer(year, day, part, "high").filterNot(_ => example)
    val low = getAnswer(year, day, part, "low").filterNot(_ => example)
    val incorrect = getIncorrect(year, day, part).filterNot(_ => example)
    val numeric = Try(BigInt(answer)).isSuccess

    val result = correct match
      case Some(expected) =>
        if expected == answer then
          "Correct"
        else if numeric then
          if BigInt(expected) < BigInt(answer) then
            "Too High"
          else
            "Too Low"
        else
          "Incorrect"
      case None =>
        if incorrect.contains(answer) then
          "Incorrect"
        else if numeric then
          if high.map(num => BigInt(num) <= BigInt(answer)).getOrElse(false) then
            "Too High"
          else if low.map(num => BigInt(num) >= BigInt(answer)).getOrElse(false) then
            "Too Low"
          else
            "Unknown"
        else
          "Unknown"
    println(result)

  private def modifyAnswers(year: Int, day: Int, part: Int, remove: String, add: String): Unit =
    val filename = os.pwd / "input" / s"$year" / s"${day}_answers.txt"
    if os.exists(filename) then
      val text = os.read.lines(filename)
        .filterNot(line => line.startsWith(s"part $part") && remove.r.unanchored.matches(line))
        .appended(s"part $part $add")
        .mkString("\n")
      os.write.over(filename, text)
    else
      os.write(filename, s"part $part $add")

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

  private def getExample(year: Int, day: Int, example: Int): String =
    val filename = os.pwd / "input" / s"$year" / s"${day}_example_$example.txt"
    if !os.exists(filename) then
      throw new Exception(s"Example file $filename does not exist")
    else
      os.read(filename)

  private def unlocked(year: Int, day: Int): Boolean =
    val calendar = Calendar.getInstance(TimeZone.getTimeZone("EST"))
    calendar.set(year, 11, day, 0, 0, 0)
    val unlockTime = calendar.getTimeInMillis()
    val currentTime = new Date().getTime()
    currentTime >= unlockTime

end Runner
