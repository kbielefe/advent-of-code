package advent2021
import cats.data.State
import cats.implicits.*

object Day16:
  def part1(input: String): Int =
    packet.runA(initialState(input)).value.sumVersions

  def part2(input: String): Long =
    packet.runA(initialState(input)).value.evaluate

  enum Packet:
    case Literal(version: Int, value: Long)
    case Operator(version: Int, typeId: Int, subPackets: List[Packet])

    def sumVersions: Int = this match
      case Literal(version, _)              => version
      case Operator(version, _, subPackets) => version + subPackets.map(_.sumVersions).sum

    def evaluate: Long = this match
      case Literal(_, value) => value
      case Operator(_, 0, subPackets) => subPackets.map(_.evaluate).sum
      case Operator(_, 1, subPackets) => subPackets.map(_.evaluate).product
      case Operator(_, 2, subPackets) => subPackets.map(_.evaluate).min
      case Operator(_, 3, subPackets) => subPackets.map(_.evaluate).max
      case Operator(_, 5, List(x, y)) => if x.evaluate >  y.evaluate then 1 else 0
      case Operator(_, 6, List(x, y)) => if x.evaluate <  y.evaluate then 1 else 0
      case Operator(_, 7, List(x, y)) => if x.evaluate == y.evaluate then 1 else 0
      case _ => throw Exception("Invalid packet")

  type Parsed[A] = State[ParseState, A]

  private def packet: Parsed[Packet] =
    for
      version <- int(3)
      typeId  <- int(3)
      packet  <- if typeId == 4 then literal(version) else operator(version, typeId)
    yield packet

  private def literal(version: Int): Parsed[Packet] =
    nibbles().map(value => Packet.Literal(version, value))

  private def nibbles(acc: Long = 0): Parsed[Long] =
    for
      keepGoing <- bool
      digit     <- int(4)
      value      = (acc << 4) + digit
      result    <- if keepGoing then nibbles(value) else State.pure(value)
    yield result

  private def operator(version: Int, typeId: Int): Parsed[Packet] =
    for
      usingCount <- bool
      n          <- if usingCount then int(11) else int(15)
      subPackets <- if usingCount then packet.replicateA(n) else packetsByLength(n)
    yield Packet.Operator(version, typeId, subPackets)

  private def packetsByLength(n: Int): Parsed[List[Packet]] =
    for
      startPos <- pos
      packets  <- packet.whileM[Vector](pos.map(_ < startPos + n))
    yield packets.toList

  private def pos: Parsed[Int] = State.inspect(_.pos)

  private def bool: Parsed[Boolean] = get(1).map(_ == "1")

  private def int(bits: Int): Parsed[Int] = get(bits).map(Integer.parseInt(_, 2))

  private def get(bits: Int): Parsed[String] =
    State{s =>
      val (output, newBits) = s.bits.splitAt(bits)
      val newPos = s.pos + bits
      (ParseState(newBits, newPos), output)
    }

  case class ParseState(bits: String, pos: Int)

  def initialState(hex: String): ParseState =
    val bits = hex.trim.flatMap{digit =>
      val int = Integer.parseInt(digit.toString, 16)
      val bin = Integer.toBinaryString(int)
      "0" * (4 - bin.size) + bin
    }.mkString
    ParseState(bits, 0)
