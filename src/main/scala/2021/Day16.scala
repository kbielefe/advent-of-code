package advent2021
import annotation.tailrec

object Day16:
  def part1(input: String): Int =
    Packet.parse(bitsIterator(input.trim)).versionSum

  def part2(input: String): Long =
    Packet.parse(bitsIterator(input.trim)).evaluate

  private def bitsIterator(hex: String): Iterator[Int] =
    hex.iterator.flatMap{digit =>
      val int = Integer.parseInt(digit.toString, 16)
      val bin = Integer.toBinaryString(int)
      "0" * (4 - bin.size) + bin
    }.map(_.asDigit)

  extension(it: Iterator[Int])
    def int(bits: Int): Int =
      (1 to bits).foldLeft(0)((acc, _) => (acc << 1) + it.next())

    def bool: Boolean = it.next() == 1

    def long(bits: Int): Long =
      (1 to bits).foldLeft(0L)((acc, _) => (acc << 1) + it.next())

  enum Packet(val version: Int, val length: Int):
    case Literal(override val version: Int, override val length: Int, value: Long) extends Packet(version, length)
    case Operator(override val version: Int, typeId: Int, override val length: Int, subPackets: List[Packet]) extends Packet(version, length)

    def versionSum: Int = this match
      case Literal(version, _, _)              => version
      case Operator(version, _, _, subPackets) => version + subPackets.map(_.versionSum).sum

    def evaluate: Long = this match
      case Literal(_, _, value)          => value
      case Operator(_, 0, _, subPackets) => subPackets.map(_.evaluate).sum
      case Operator(_, 1, _, subPackets) => subPackets.map(_.evaluate).product
      case Operator(_, 2, _, subPackets) => subPackets.map(_.evaluate).min
      case Operator(_, 3, _, subPackets) => subPackets.map(_.evaluate).max
      case Operator(_, 5, _, List(x, y)) => if x.evaluate >  y.evaluate then 1 else 0
      case Operator(_, 6, _, List(x, y)) => if x.evaluate <  y.evaluate then 1 else 0
      case Operator(_, 7, _, List(x, y)) => if x.evaluate == y.evaluate then 1 else 0
      case _ => throw Exception("Invalid packet")

  object Packet:
    def parse(it: Iterator[Int]): Packet =
      val version = it.int(3)
      val typeId = it.int(3)
      typeId match
        case 4 =>
          val (length, literal) = parseLiteral(it)
          Literal(version, length + 6, literal)
        case t =>
          val lengthInSubPackets = it.bool
          val subPackets = parseSubPackets(lengthInSubPackets, it)
          val length = 7 + (if lengthInSubPackets then 11 else 15)
          Operator(version, t, length + subPackets.map(_.length).sum, subPackets)

    @tailrec
    private def parseLiteral(it: Iterator[Int], length: Int = 0, result: Long = 0): (Int, Long) =
      val keepReading = it.bool
      val newResult = (result << 4) + it.long(4)
      if keepReading then
        parseLiteral(it, length + 5, newResult)
      else
        (length + 5, newResult)

    private def parseSubPackets(lengthInSubPackets: Boolean, it: Iterator[Int]): List[Packet] =
      if lengthInSubPackets then
        val numberOfSubPackets = it.int(11)
        parseNSubPackets(numberOfSubPackets, it)
      else
        val lengthInBits = it.int(15)
        parseLengthSubPackets(lengthInBits, it)

    private def parseNSubPackets(n: Int, it: Iterator[Int]): List[Packet] =
      if n == 1 then
        List(parse(it))
      else
        parse(it) :: parseNSubPackets(n - 1, it)

    @tailrec
    private def parseLengthSubPackets(length: Int, it: Iterator[Int], result: List[Packet] = List.empty): List[Packet] =
      if length == 0 then
        result.reverse
      else
        val packet = parse(it)
        parseLengthSubPackets(length - packet.length, it, packet :: result)
