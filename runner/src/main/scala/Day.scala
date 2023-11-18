package runner
import parse.*

private[runner] trait NormalizedDay:
  def normalizedPart1(input: String): String
  def normalizedPart2(input: String): String

trait Day[I: Read, A: Show, B: Show] extends NormalizedDay:
  def part1(input: I): A
  def part2(input: I): B

  def normalizedPart1(input: String): String =
    val stringInput = summon[Read[I]].read(input)
    summon[Show[A]].show(time(part1(stringInput)))

  def normalizedPart2(input: String): String =
    val stringInput = summon[Read[I]].read(input)
    summon[Show[B]].show(time(part2(stringInput)))

  def time[A](f: => A): A =
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    println(s"${(end - start)/1e6} ms")
    result
