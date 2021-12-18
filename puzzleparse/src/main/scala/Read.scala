package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.ClassTag
import scala.util.matching.Regex

enum Level derives CanEqual:
  case Char, Word, Line, Multiline

trait Read[+A]:
  def read(input: String): Either[String, A]

given [A : ReadParser]: Read[A] with
  def read(input: String): Either[String, A] =
    val level = if input.contains("\n\n") then
      Level.Multiline
    else if input.contains("\n") then
      Level.Line
    else
      Level.Word
    summon[ReadParser[A]].parser(level).parse(input).toEither

object Read:
  inline given derived[T](using mirror: Mirror.ProductOf[T]): Read[T] =
    new Read[T]:
      val reader = summonInline[Read[mirror.MirroredElemTypes]]
      override def read(input: String): Either[String, T] =
        reader.read(input).map(mirror.fromProduct)

trait ReadParser[+A]:
  def parser(level: Level): Parser[A]

given ReadParser[EmptyTuple] with
  def parser(level: Level): Parser[EmptyTuple] =
    Parser.success(EmptyTuple)

given [H : ReadParser, T <: Tuple : ReadParser]: ReadParser[H *: T] with
  def parser(level: Level): Parser[H *: T] = ???

given ReadParser[Int] with
  def parser(level: Level): Parser[Int] = level match
    case Level.Char => Parser.r("\\d").map(_.toInt)
    case _          => Parser.r("-?\\d+").map(_.toInt)

given ReadParser[String] with
  def parser(level: Level): Parser[String] =
    Parser.r(".*")

trait Parser[+A]:
  def parse(input: Input): ParseResult[A]

  def parse(input: String): ParseResult[A] = parse(Input(input))

  def map[B](f: A => B): Parser[B] =
    val self = this
    new Parser[B]:
      def parse(input: Input): ParseResult[B] =
        self.parse(input).map(f)

end Parser

object Parser:
  def r(pattern: String): Parser[String] = new Parser[String]:
    val regex = pattern.r
    def parse(input: Input): ParseResult[String] =
      regex.findFirstMatchIn(input.string).map{m =>
        ParseResult.Success(m.matched, input) // TODO: advance the input
      }.getOrElse(ParseResult.Error(pattern, input))

  def success[A](value: A): Parser[A] = new Parser[A]:
    def parse(input: Input): ParseResult[A] =
      ParseResult.Success(value, input)
end Parser

sealed trait ParseResult[+A]:
  def toEither: Either[String, A]
  def map[B](f: A => B): ParseResult[B]

object ParseResult:
  case class Success[+A](a: A, input: Input) extends ParseResult[A]:
    def toEither: Either[String, A] = Right(a)
    def map[B](f: A => B): ParseResult[B] =
      Success(f(a), input)

  case class Error(expected: String, input: Input) extends ParseResult[Nothing]:
    def toEither: Either[String, Nothing] = Left(s"Parse Error: expected $expected, but got $input")
    def map[B](f: Nothing => B): ParseResult[B] = this

case class Input(string: String, line: Int = 0, col: Int = 0)
