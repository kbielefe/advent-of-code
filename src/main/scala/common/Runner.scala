import java.awt.datatransfer.StringSelection
import java.awt.Toolkit
import scala.io.Source

object Runner:
  @main def main(year: Int, day: Int, part: Int): Unit =
    val input = Source.fromFile(s"input/$year/$day.txt").mkString
    val run = runDay(input, part)
    (year, day) match
      case (2020, 1) => run(advent2020.Day1.part1, advent2020.Day1.part2)
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
end Runner
