package day12
import parse.{*, given}
import io.circe.Json

object Puzzle extends runner.Day[Json, Int, Int]:
  given CanEqual[Json, Json] = CanEqual.derived

  def part1(input: Json): Int =
    traverseNumbers(input, false)

  def part2(input: Json): Int =
    traverseNumbers(input, true)

  private def traverseNumbers(json: Json, skipRed: Boolean): Int =
    json.fold(
      0,
      _ => 0,
      _.toInt.get,
      _ => 0,
      _.map(traverseNumbers(_, skipRed)).sum,
      obj => if skipRed && obj.values.exists(_ == Json.fromString("red")) then 0 else obj.values.map(traverseNumbers(_, skipRed)).sum
    )
