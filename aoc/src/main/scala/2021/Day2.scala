package advent2021
import puzzleparse.{*, given}

object Day2:
  def part1(input: List[(Letters, Int)]): Int =
    val (horizontal, depth) = input
      .foldLeft((0, 0)){case ((horizontal, depth), (command, amount)) =>
        command match
          case "forward" => (horizontal + amount, depth)
          case "up"      => (horizontal, depth - amount)
          case "down"    => (horizontal, depth + amount)
      }
    horizontal * depth

  def part2(input: List[(Letters, Int)]): Int =
    val (horizontal, depth, _) = input
      .foldLeft((0, 0, 0)){case ((horizontal, depth, aim), (command, amount)) =>
        command match
          case "forward" => (horizontal + amount, depth + aim * amount, aim)
          case "up"      => (horizontal, depth, aim - amount)
          case "down"    => (horizontal, depth, aim + amount)
      }
    horizontal * depth
