package advent2021
import scala.annotation.tailrec

object Day10:
  def part1(input: List[String]): Int =
    input.flatMap(line => validate(line).left.toOption).map{
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }.sum

  def part2(input: List[String]): Long =
    val scores = input.flatMap(line => validate(line).toOption).map(score).sorted
    scores(scores.size / 2)

  private def score(completion: String): Long =
    completion.foldLeft(0L){(acc, char) =>
      val charScore = char match
        case '(' => 1
        case '[' => 2
        case '{' => 3
        case '<' => 4
      5 * acc + charScore
    }

  private val pairs = Map(
    '{' -> '}',
    '<' -> '>',
    '[' -> ']',
    '(' -> ')'
  )

  @tailrec
  private def validate(line: String, stack: List[Char] = List.empty): Either[Char, String] =
    if line.isEmpty then
      Right(stack.mkString)
    else if "{(<[".contains(line.head) then
      validate(line.tail, line.head :: stack)
    else if stack.isEmpty || pairs(stack.head) != line.head then
      Left(line.head)
    else
      validate(line.tail, stack.tail)
