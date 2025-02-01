package advent2016

object Day7:
  def part1(input: List[String]): Int =
    input.count(supportsTLS)

  def part2(input: List[String]): Int =
    input.count(supportsSSL)

  private val inside = """\[([^\]]+)\]""".r
  private val outside = """(^|\])([^\[]+)""".r

  private def supportsTLS(ip: String): Boolean =
    val insideBrackets = inside.findAllMatchIn(ip).map(_.group(1))
    val outsideBrackets = outside.findAllMatchIn(ip).map(_.group(2))
    val insideExists = insideBrackets.exists(abba)
    val outsideExists = outsideBrackets.exists(abba)
    !insideExists && outsideExists

  private def supportsSSL(ip: String): Boolean =
    val insideBrackets = inside.findAllMatchIn(ip).map(_.group(1)).toSet.flatMap(aba)
    val outsideBrackets = outside.findAllMatchIn(ip).map(_.group(2)).toSet.flatMap(aba).map(bab)
    !(insideBrackets & outsideBrackets).isEmpty

  private def abba(input: String): Boolean =
    input.toList.sliding(4).exists{case List(a, b, c, d) => a == d && b == c && a != b}

  private def aba(input: String): Set[String] =
    input.toList.sliding(3).filter{case List(a, b, c) => a == c && a != b}.toSet.map(_.mkString)

  private def bab(input: String): String =
    s"${input(1)}${input(0)}${input(1)}"
