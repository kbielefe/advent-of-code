import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.util.{Calendar, Date, TimeZone}
import puzzleparse.{*, given}
import scala.util.Try
import sttp.client3.*

case class Puzzle(year: Int, day: Int, part: Int, command: Option[String], example: Option[Int], exampleAnswer: Option[String])

object Puzzle:
  def apply(year: Int, day: Int, part: Int, optional: Seq[String]): Puzzle =
    val command = optional.headOption.filter(_ != "example")
    val example = optional.headOption.filter(_ == "example").map(_ => optional(1).toInt)
    val exampleAnswer = if example.isDefined && optional.length > 2 then Some(optional(2)) else None
    Puzzle(year, day, part, command, example, exampleAnswer)

object Runner:
  @main def advent(year: Int, day: Int, part: Int, optional: String*): Unit =
    val puzzle = Puzzle(year, day, part, optional)
    if puzzle.exampleAnswer.isDefined then
      exampleAnswer(puzzle)
    if !unlocked(puzzle) then
      println("Puzzle input not yet unlocked")
      return
    val run = runDay(puzzle)
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
      case (2021, 5) => run(advent2021.Day5.part1, advent2021.Day5.part2)
      case _         => println("Puzzle solution not found.")

  private class runDay(puzzle: Puzzle):
    val input = puzzle.example.map(_ => getExample(puzzle)).getOrElse(getOrDownloadInput(puzzle))

    def apply[A: Read, B: Show, C: Read, D: Show](part1: A => B, part2: C => D): Unit =
      val start = System.currentTimeMillis()
      val output = if puzzle.part == 1 then run(part1) else run(part2)
      val end = System.currentTimeMillis()
      println(s"(${end - start} ms)\n$output")
      copyToClipboard(output)
      runCommand(puzzle, output)

    private def run[A, B](f: A => B)(using read: Read[A], show: Show[B]): String =
      show.show(f(read.read(input)))

    private def copyToClipboard(text: String): Unit =
      Toolkit
        .getDefaultToolkit()
        .getSystemClipboard()
        .setContents(new StringSelection(text), null)
  end runDay

  private def runCommand(puzzle: Puzzle, output: String): Unit =
    val command = puzzle.command.getOrElse("")
    if command.startsWith("correct") then
      markCorrect(puzzle, output)
    if command.startsWith("incorrect") then
      markIncorrect(puzzle, output)
    else if command.startsWith("high") then
      markHigh(puzzle, output)
    else if command.startsWith("low") then
      markLow(puzzle, output)
    else
      checkAnswer(puzzle, output)

  private def getAnswer(puzzle: Puzzle, kind: String): Option[String] =
    val filename = os.pwd / "input" / s"${puzzle.year}" / s"${puzzle.day}_answers.txt"
    if os.exists(filename) then
      os.read.lines(filename)
        .find(_.startsWith(s"part ${puzzle.part} $kind"))
        .map(_.split(" ").last)
    else None

  private def getIncorrect(puzzle: Puzzle): IndexedSeq[String] =
    val filename = os.pwd / "input" / s"${puzzle.year}" / s"${puzzle.day}_answers.txt"
    if os.exists(filename) then
      os.read.lines(filename)
        .filter(_.startsWith(s"part ${puzzle.part} incorrect"))
      .map(_.split(" ").last)
    else IndexedSeq.empty

  private def exampleAnswer(puzzle: Puzzle): Unit =
    modifyAnswers(puzzle, s"example ${puzzle.example.get}", s"example ${puzzle.example.get} ${puzzle.exampleAnswer.get}")

  private def markCorrect(puzzle: Puzzle, answer: String): Unit =
    modifyAnswers(puzzle, s"correct|incorrect|low|high", s"correct $answer")

  private def markIncorrect(puzzle: Puzzle, answer: String): Unit =
    modifyAnswers(puzzle, s"incorrect $answer", s"incorrect $answer")

  private def markHigh(puzzle: Puzzle, answer: String): Unit =
    val correct = getAnswer(puzzle, "correct")
    val high = getAnswer(puzzle, "high")
    if !correct.isDefined && high.map(num => BigInt(num) < BigInt(answer)).getOrElse(true) then
      modifyAnswers(puzzle, s"high", s"high $answer")

  private def markLow(puzzle: Puzzle, answer: String): Unit =
    val correct = getAnswer(puzzle, "correct")
    val low = getAnswer(puzzle, "low")
    if !correct.isDefined && low.map(num => BigInt(num) > BigInt(answer)).getOrElse(true) then
      modifyAnswers(puzzle, s"low", s"low $answer")

  private def checkAnswer(puzzle: Puzzle, answer: String): Unit =
    val example = puzzle.example.isDefined
    val correctString = puzzle.example.map(example => s"example $example").getOrElse("correct")
    val correct = getAnswer(puzzle, correctString)
    val high = getAnswer(puzzle, "high").filterNot(_ => example)
    val low = getAnswer(puzzle, "low").filterNot(_ => example)
    val incorrect = getIncorrect(puzzle).filterNot(_ => example)
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

  private def modifyAnswers(puzzle: Puzzle, remove: String, add: String): Unit =
    val filename = os.pwd / "input" / s"${puzzle.year}" / s"${puzzle.day}_answers.txt"
    if os.exists(filename) then
      val text = os.read.lines(filename)
        .filterNot(line => line.startsWith(s"part ${puzzle.part}") && remove.r.unanchored.matches(line))
        .appended(s"part ${puzzle.part} $add")
        .mkString("\n")
      os.write.over(filename, text)
    else
      os.write(filename, s"part ${puzzle.part} $add")

  private def getOrDownloadInput(puzzle: Puzzle): String =
    val filename = os.pwd / "input" / s"${puzzle.year}" / s"${puzzle.day}.txt"
    if os.exists(filename) then
      os.read(filename)
    else
      println("Downloading new input")
      val session = os.read(os.pwd / "input" / "session").trim
      val backend = HttpURLConnectionBackend()
      val response = basicRequest
        .cookie("session", session)
        .get(uri"https://adventofcode.com/${puzzle.year}/day/${puzzle.day}/input")
        .send(backend)
      val text = response.body match
        case Left(e) => throw new Exception(s"Error downloading input: $e")
        case Right(text) => text
      os.write(filename, text)
      text

  private def getExample(puzzle: Puzzle): String =
    val filename = os.pwd / "input" / s"${puzzle.year}" / s"${puzzle.day}_example_${puzzle.example.get}.txt"
    if !os.exists(filename) then
      throw new Exception(s"Example file $filename does not exist")
    else
      os.read(filename)

  private def unlocked(puzzle: Puzzle): Boolean =
    val calendar = Calendar.getInstance(TimeZone.getTimeZone("EST"))
    calendar.set(puzzle.year, 11, puzzle.day, 0, 0, 0)
    val unlockTime = calendar.getTimeInMillis()
    val currentTime = new Date().getTime()
    currentTime >= unlockTime

end Runner
