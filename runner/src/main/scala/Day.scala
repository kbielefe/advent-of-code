package runner
import parse.*

private[runner] trait NormalizedDay:
  def normalizedPart1(input: String): String
  def normalizedPart2(input: String): String

trait Day[I: Read, A: Show, B: Show] extends NormalizedDay:
  def part1(input: I): A
  def part2(input: I): B
  def normalizedPart1(input: String): String =
    summon[Show[A]].show(part1(summon[Read[I]].read(input)))
  def normalizedPart2(input: String): String =
    summon[Show[B]].show(part2(summon[Read[I]].read(input)))
