import scala.io.Source

object Runner:
  @main def main(year: Int, day: Int, part: Int): Unit =
    val run = runDay(year, day)
    (year, day) match
      case (2020, 1) => run(advent2020.Day1.part1, advent2020.Day1.part2, part)
      case _         => println("Puzzle solution not found.")

  private class runDay(year: Int, day: Int):
    def apply[A: Read, B: Show, C: Read, D: Show](part1: A => B, part2: C => D, part: Int): Unit =
      val input = Source.fromFile(s"input/$year/$day.txt").mkString
      val start = System.currentTimeMillis()
      val output = if part == 1 then
        summon[Show[B]].show(part1(summon[Read[A]].read(input)))
      else
        summon[Show[D]].show(part2(summon[Read[C]].read(input)))
      val end = System.currentTimeMillis()
      println(s"Year $year, Day $day, Part $part (${end - start} ms): $output")
  end runDay
end Runner
