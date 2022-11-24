package advent2016

object Day4:
  def part1(input: List[String]): Int =
    input.map(parse).filter(valid.tupled).map(_._2).sum

  def part2(input: List[String]): Int =
    input.map(parse).filter(valid.tupled).map(decrypt.tupled).find(_._1.contains("north")).get._2

  private val regex = """([a-z\-]+)(\d+)\[([a-z]+)\]""".r

  private def parse(input: String): (String, Int, String) =
    val regex(name, id, checksum) = input: @unchecked
    (name, id.toInt, checksum)

  private def valid(name: String, id: Int, checksum: String): Boolean =
    val freqs = name.filterNot(_ == '-').groupMapReduce(identity)(_ => 1)(_ + _)
    val actualChecksum = freqs.toSeq.sortBy(x => (-x._2, x._1)).take(5).map(_._1).mkString
    actualChecksum == checksum

  private def decrypt(name: String, id: Int, checksum: String): (String, Int) =
    name.map(char => if char == '-' then ' ' else ((char + id - 'a') % 26 + 'a').toChar) -> id
