package day14
import parse.{*, given}
import java.security.MessageDigest

object Puzzle extends runner.Day[String, Int, Int]:
  def part1(input: String): Int =
    result(singleHashes(input.trim))

  def part2(input: String): Int =
    result(multiHashes(input.trim))

  def result(hashes: Iterator[String]): Int =
    val (quintHashes, tripleHashes) = hashes.duplicate

    val quints = multiples(5, quintHashes).zipWithIndex.scanLeft(Map.empty[Char, Int]){
      case (map, (Some(char), index)) => map + (char -> index)
      case (map, _) => map
    }
    val keys = multiples(3, tripleHashes).zipWithIndex.zip(quints.drop(1001)).collect{case ((Some(char), index), map) if map.getOrElse(char, -1) > index => index}
    keys.drop(63).next

  private val md = MessageDigest.getInstance("MD5")

  private def hashSalt(salt: String)(index: Int): Array[Byte] =
    md.reset()
    md.digest(s"$salt$index".getBytes)

  private def hash(value: String): String =
    md.reset()
    md.digest(value.getBytes).map("%02x" format _).mkString

  private def singleHashes(salt: String) =
    Iterator
      .from(0)
      .map(hashSalt(salt))
      .map(_.map("%02x" format _).mkString)

  private def multiHashes(salt: String) =
    Iterator
      .from(0)
      .map(index => Iterator.iterate(s"$salt$index")(hash).drop(2017).next)

  private def multiples(of: Int, hashes: Iterator[String]): Iterator[Option[Char]] =
    val regex = s"""(.)\\1{${of - 1}}""".r
      hashes.map(hash => regex.findFirstIn(hash).map(_.head))
