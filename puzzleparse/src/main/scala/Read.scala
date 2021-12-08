package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.ClassTag
import scala.util.matching.Regex.Match

enum Level derives CanEqual:
  case Char, Line, Multiline

trait Read[A]:
  def read(input: String): A =
    val level = if input.contains("\n\n") then
      Level.Multiline
    else if input.contains("\n") then
      Level.Line
    else
      Level.Char
    val prefix = if level == Level.Multiline || level == Level.Line then "(?s)" else ""
    val m = (prefix + pattern(level)).r.findFirstMatchIn(input).get
    extract(m, 1, level)
  def pattern(level: Level): String
  def extract(m: Match, group: Int, level: Level): A
  def groupCount: Int

given Read[Int] with
  def pattern(level: Level): String = "(-?\\d+)"
  def extract(m: Match, group: Int, level: Level): Int =
    m.group(group).toInt
  def groupCount: Int = 1

given Read[String] with
  def pattern(level: Level): String = level match
    case Level.Char      => "(.+)"
    case Level.Line      => "([^\\n]+)"
    case Level.Multiline => """([^\n]+(\n[^\n]+)*)"""
  def extract(m: Match, group: Int, level: Level): String =
    m.group(group).trim
  def groupCount: Int = 1

given Read[EmptyTuple] with
  override def read(input: String): EmptyTuple = EmptyTuple
  def pattern(level: Level): String = ""
  def extract(m: Match, group: Int, level: Level): EmptyTuple = EmptyTuple
  def groupCount: Int = 0

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  inline def pattern(level: Level): String =
    val sep = inline erasedValue[T] match
      case _: EmptyTuple => ""
      case _ => ".*?"
    summon[Read[H]].pattern(level) + sep + summon[Read[T]].pattern(level)

  def extract(m: Match, group: Int, level: Level): H *: T =
    val headReader = summon[Read[H]]
    headReader.extract(m, group, level) *: summon[Read[T]].extract(m, group + headReader.groupCount, level)

  def groupCount: Int =
    summon[Read[H]].groupCount + summon[Read[T]].groupCount

given [A](using reader: Read[A]): Read[List[A]] with
  inline def pattern(level: Level): String =
    val delim = level match
      case Level.Char      => ".*"
      case Level.Line      => "\\n"
      case Level.Multiline => "\\n\\n"
    "(" + reader.pattern(level) + "?(" + delim + reader.pattern(level) + ")*)"

  def extract(m: Match, group: Int, level: Level): List[A] =
    val matches = ("(?s)" + reader.pattern(level)).r.findAllMatchIn(m.group(group))
    matches.map(m => reader.extract(m, 1, level)).toList

  def groupCount: Int =
    1 + 2 * reader.groupCount

object Read:
  inline given derived[T](using mirror: Mirror.ProductOf[T]): Read[T] =
    new Read[T]:
      val reader = summonInline[Read[mirror.MirroredElemTypes]]
      override def read(input: String): T = mirror.fromProduct(reader.read(input))
      def pattern(level: Level): String = reader.pattern(level)
      def extract(m: Match, group: Int, level: Level): T = mirror.fromProduct(reader.extract(m, group, level))
      def groupCount: Int = reader.groupCount
