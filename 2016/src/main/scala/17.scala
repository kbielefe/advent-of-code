package advent2016
import java.security.MessageDigest

object Day17:
  def part1(input: String): String =
    dfs(input.trim, "", (0, 0)).minBy(_.length)

  def part2(input: String): Int =
    dfs(input.trim, "", (0, 0)).map(_.length).max

  private val md = MessageDigest.getInstance("MD5")

  private def hash(value: String): String =
    md.reset()
    md.digest(value.getBytes).map("%02x" format _).mkString

  private def dfs(salt: String, path: String, pos: (Int, Int)): Set[String] =
    if pos == (3, 3) then
      Set(path)
    else
      neighbors(salt, path, pos).map{direction =>
        val (x, y) = pos
        val newPos = direction match
          case 'U' => (x, y - 1)
          case 'D' => (x, y + 1)
          case 'L' => (x - 1, y)
          case 'R' => (x + 1, y)
        val newPath = path + direction
        dfs(salt, newPath, newPos)
      }.flatten

  private def neighbors(salt: String, path: String, pos: (Int, Int)): Set[Char] =
    hash(salt + path)
      .take(4)
      .zip("UDLR")
      .filter(open)
      .map(_._2)
      .filter(inbounds(pos))
      .toSet

  private def open(x: (Char, Char)): Boolean =
    "bcdef".contains(x._1)

  private def inbounds(pos: (Int, Int))(direction: Char): Boolean =
    direction match
      case 'U' => pos._2 > 0
      case 'D' => pos._2 < 3
      case 'L' => pos._1 > 0
      case 'R' => pos._1 < 3
