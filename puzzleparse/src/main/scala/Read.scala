package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.reflect.ClassTag
import scala.util.matching.Regex.Match

trait Read[A]:
  def read(input: String): A
  def pattern: String
  def extract(m: Match, group: Int): A

given Read[Int] with
  def read(input: String): Int =
    pattern.r.findFirstIn(input).map(_.toInt).get
  def pattern: String = "-?\\d+"
  def extract(m: Match, group: Int): Int =
    m.group(group).toInt

given Read[String] with
  def read(input: String): String =
    input
  def pattern: String = "(?s).*"
  def extract(m: Match, group: Int): String =
    m.group(group).trim

given Read[EmptyTuple] with
  def read(input: String): EmptyTuple = EmptyTuple
  def pattern: String = ""
  def extract(m: Match, group: Int): EmptyTuple = EmptyTuple

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  def read(input: String): H *: T =
    val m = pattern.r.findFirstMatchIn(input).get
    extract(m, 1)

  inline def pattern: String =
    val sep = inline erasedValue[T] match
      case _: EmptyTuple => ""
      case _ => ".*?"
    "(" + summon[Read[H]].pattern + ")" + sep + summon[Read[T]].pattern

  def extract(m: Match, group: Int): H *: T =
    summon[Read[H]].extract(m, group) *: summon[Read[T]].extract(m, group + 1)

object Read:
  inline given derived[T](using m: Mirror.ProductOf[T]): Read[T] =
    new Read[T]:
      def read(input: String): T =
        m.fromProduct(fold[m.MirroredElemTypes](input))
      def pattern: String = ???
      def extract(m: Match, group: Int): T = ???

  private inline def fold[T <: Tuple](input: String): Tuple =
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts)  => ???
