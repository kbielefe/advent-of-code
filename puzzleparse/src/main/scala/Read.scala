package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.ClassTag
import scala.util.matching.Regex.Match

trait Read[A]:
  def read(input: String): A =
    val m = pattern.r.findFirstMatchIn(input).get
    extract(m, 1)
  def pattern: String
  def extract(m: Match, group: Int): A
  def groupCount: Int

given Read[Int] with
  def pattern: String = "(-?\\d+)"
  def extract(m: Match, group: Int): Int =
    m.group(group).toInt
  def groupCount: Int = 1

given Read[String] with
  def pattern: String = "(?s)(.*)"
  def extract(m: Match, group: Int): String =
    m.group(group).trim
  def groupCount: Int = 1

given Read[EmptyTuple] with
  override def read(input: String): EmptyTuple = EmptyTuple
  def pattern: String = ""
  def extract(m: Match, group: Int): EmptyTuple = EmptyTuple
  def groupCount: Int = 0

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  inline def pattern: String =
    val sep = inline erasedValue[T] match
      case _: EmptyTuple => ""
      case _ => ".*?"
    summon[Read[H]].pattern + sep + summon[Read[T]].pattern

  def extract(m: Match, group: Int): H *: T =
    val headReader = summon[Read[H]]
    headReader.extract(m, group) *: summon[Read[T]].extract(m, group + headReader.groupCount)

  def groupCount: Int =
    summon[Read[H]].groupCount + summon[Read[T]].groupCount

object Read:
  inline given derived[T](using mirror: Mirror.ProductOf[T]): Read[T] =
    new Read[T]:
      val reader = summonInline[Read[mirror.MirroredElemTypes]]
      override def read(input: String): T = mirror.fromProduct(reader.read(input))
      def pattern: String = reader.pattern
      def extract(m: Match, group: Int): T = mirror.fromProduct(reader.extract(m, group))
      def groupCount: Int = reader.groupCount
