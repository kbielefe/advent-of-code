package advent2021
import cats.data.State
import cats.implicits.*
import scala.annotation.tailrec

object Day18:
  def part1(input: List[String]): Long =
    val snailfish = input.map(Snailfish.snailfish.runA(_).value)
    snailfish.reduceLeft(_ + _).magnitude

  def part2(input: List[String]): Long =
    val snailfish = input.map(Snailfish.snailfish.runA(_).value)
    snailfish
      .combinations(2)
      .flatMap{case List(x, y) => List(x + y, y + x)}
      .map(_.magnitude)
      .max

  enum Snailfish derives CanEqual:
    case Pair(left: Snailfish, right: Snailfish)
    case Literal(value: Long)

    override def toString: String = this match
      case Pair(left, right) => s"[$left,$right]"
      case Literal(value)    => value.toString

    def flatten: Iterator[(Long, Int, Int)] =
      def helper(fish: Snailfish, depth: Int): Iterator[(Long, Int)] = fish match
        case Pair(left, right) => helper(left, depth + 1) ++ helper(right, depth + 1)
        case Literal(value)    => Iterator((value, depth - 1))
      helper(this, 0).zipWithIndex.map{case ((v, d), i) => (v, d, i)}

    def +(other: Snailfish): Snailfish = Pair(this, other).reduce
    def magnitude: Long = this match
      case Literal(value)    => value
      case Pair(left, right) => 3 * left.magnitude + 2 * right.magnitude

    @tailrec
    private def reduce: Snailfish =
      explode.orElse(split) match
        case None            => this
        case Some(snailfish) => snailfish.reduce

    private def explode: Option[Snailfish] =
      flatten.filter(_._2 == 4).take(2).toSeq match
        case Seq((left, _, leftIndex), (right, _, rightIndex)) => Some(addLeft(left, leftIndex).addRight(right, rightIndex).replacePairWithZero(leftIndex))
        case _ => None

    private def addLeft(value: Long, index: Int): Snailfish =
      flatten.takeWhile(_._3 < index).toSeq.lastOption match
        case Some(_, _, i) => addToIndex(value, i)
        case None => this

    private def addRight(value: Long, index: Int): Snailfish =
      flatten.dropWhile(_._3 <= index).toSeq.headOption match
        case Some(_, _, i) => addToIndex(value, i)
        case None => this

    private def addToIndex(add: Long, index: Int): Snailfish =
      def helper(currentIndex: Int, fish: Snailfish): (Int, Snailfish) =
        fish match
          case Pair(left, right) =>
            val (leftIndex, leftFish) = helper(currentIndex, left)
            val (rightIndex, rightFish) = helper(leftIndex, right)
            (rightIndex, Pair(leftFish, rightFish))
          case Literal(value) => (currentIndex + 1, Literal(if currentIndex == index then value + add else value))
      helper(0, this)._2

    private def replacePairWithZero(index: Int): Snailfish =
      def helper(currentIndex: Int, depth: Int, fish: Snailfish): (Int, Snailfish) =
        fish match
          case Pair(left, right) =>
            val (leftIndex, leftFish) = helper(currentIndex, depth + 1, left)
            val (rightIndex, rightFish) = helper(leftIndex, depth + 1, right)
            (rightIndex, if currentIndex == index && depth == 4 then Literal(0) else Pair(leftFish, rightFish))
          case Literal(value) => (currentIndex + 1, Literal(value))
      helper(0, 0, this)._2

    private def split: Option[Snailfish] =
      this match
        case Literal(value) if value >= 10 => Some(Pair(Literal(value / 2), Literal(value / 2 + value % 2)))
        case _: Literal        => None
        case Pair(left, right) => (left.split, right.split) match
          case (None, None)    => None
          case (Some(s), _)    => Some(Pair(s, right))
          case (None, Some(s)) => Some(Pair(left, s))

  object Snailfish:
    def snailfish: State[String, Snailfish] = for
      char   <- peek
      result <- if char == '[' then pair else literal
    yield result

    def pair: State[String, Pair] = for
      _     <- drop
      left  <- snailfish
      _     <- drop
      right <- snailfish
      _     <- drop
    yield Pair(left, right)

    def literal: State[String, Literal] = State{s =>
      (s.dropWhile(_.isDigit), Literal(s.takeWhile(_.isDigit).toLong))
    }

    def drop: State[String, Unit] = State.modify(_.drop(1))
    def peek: State[String, Char] = State.inspect(_.head)
