package advent2016
import java.security.MessageDigest

object Day5:
  def part1(input: String): String =
    Iterator
      .from(0)
      .map(hash(input.trim))
      .filter(interesting)
      .map(_(2))
      .map(unsignedHex)
      .take(8)
      .mkString

  def part2(input: String): String =
    Iterator
      .from(0)
      .map(hash(input.trim))
      .filter(interesting)
      .filter(_(2) < 8)
      .map(bytes => (bytes(2), (bytes(3) >> 4 & 0x0f).toHexString))
      .scanLeft(Map.empty[Byte, String]){case (map, (pos, char)) =>
        if map.contains(pos) then
          map
        else
          map + (pos -> char)
      }
      .dropWhile(_.size < 8)
      .map(_.toList.sortBy(_._1).map(_._2).mkString)
      .next

  private val md = MessageDigest.getInstance("MD5")

  private def hash(salt: String)(index: Int): Array[Byte] =
    md.reset()
    md.digest(s"$salt$index".getBytes)

  private def unsignedHex(byte: Byte): String = byte.toInt.toHexString

  private def interesting(bytes: Array[Byte]): Boolean =
    bytes(0) == 0 && bytes(1) == 0 && (bytes(2) & 0xf0) == 0
