package advent2022
import puzzleparse.{*, given}

object Day2:
  def part1(input: List[String]): Int =
    input.map(score1).sum

  def part2(input: List[String]): Int =
    input.map(score2).sum

  private def score1(moves: String): Int = moves match
    case "A X" => 3 + 1
    case "A Y" => 6 + 2
    case "A Z" => 0 + 3
    case "B X" => 0 + 1
    case "B Y" => 3 + 2
    case "B Z" => 6 + 3
    case "C X" => 6 + 1
    case "C Y" => 0 + 2
    case "C Z" => 3 + 3
    case _ => throw new Exception("Unknown input " + moves)

  private def score2(moves: String): Int = moves match
    case "A X" => 0 + 3
    case "A Y" => 3 + 1
    case "A Z" => 6 + 2
    case "B X" => 0 + 1
    case "B Y" => 3 + 2
    case "B Z" => 6 + 3
    case "C X" => 0 + 2
    case "C Y" => 3 + 3
    case "C Z" => 6 + 1
    case _ => throw new Exception("Unknown input " + moves)
