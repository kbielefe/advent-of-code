package day4
import parse.{*, given}

case class Room(encryptedName: String, sectorId: Int, checksum: String):
  def isReal: Boolean =
    val lettersBySize = encryptedName.filterNot(_ == '-').groupBy(identity).view.mapValues(_.size)
    val sortedByFrequency = lettersBySize.toList.sortBy((letter, frequency) => (-1 * frequency, letter))
    val expectedChecksum = sortedByFrequency.take(5).map(_._1).mkString
    expectedChecksum == checksum

  def decryptedName: String =
    encryptedName.map(rotate).mkString

  private def rotate(char: Char): Char =
    if char == '-' then
      ' '
    else
      ((char.toInt - 'a'.toInt + sectorId) % 26 + 'a'.toInt).toChar
end Room

type I = List[Room ~ """(.+)-(\d+)\[(.+)\]"""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.filter(_.isReal).map(_.sectorId).sum

  def part2(input: I): Int =
    input.find(_.decryptedName == "northpole object storage").get.sectorId
