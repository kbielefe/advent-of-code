package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.*

trait Show[A]:
  def show(output: A): String
  def list(output: A): List[String] = List(show(output))

given Show[Int] with
  def show(output: Int): String =
    output.toString

given Show[String] with
  def show(output: String): String =
    output

given Show[EmptyTuple] with
  def show(output: EmptyTuple): String =
    "()"

  override def list(output: EmptyTuple): List[String] =
    Nil

given [H : Show, T <: Tuple : Show]: Show[H *: T] with
  def show(output: H *: T): String =
    list(output).mkString("(", ", ", ")")

  override def list(output: H *: T): List[String] =
    summon[Show[H]].show(output.head) :: summon[Show[T]].list(output.tail)

given Show[Letter] with
  def show(output: Letter): String =
    output.toString

given Show[Letters] with
  def show(output: Letters): String =
    output

given Show[Char] with
  def show(output: Char): String =
    output.toString

given Show[Long] with
  def show(output: Long): String =
    output.toString

given [A : Show]: Show[MultiLine[A]] with
  def show(output: MultiLine[A]): String =
    output.map(summon[Show[A]].show).mkString("\n\n")

given [K : Show, V : Show]: Show[Map[K, V]] with
  def show(output: Map[K, V]): String =
    output.map((k, v) => s"${summon[Show[K]].show(k)}:${summon[Show[V]].show(v)}").mkString("\n")

given [A : Show]: Show[List[A]] with
  def show(output: List[A]): String =
    list(output).mkString("\n")

  override def list(output: List[A]): List[String] =
    output.map(summon[Show[A]].show)

object Show:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): Show[T] =
    new Show[T]:
      def show(output: T): String =
        val label = constValue[m.MirroredLabel]
        val elems = elemsList[m.MirroredElemLabels, m.MirroredElemTypes](Tuple.fromProductTyped(output))
        label + elems.mkString("(", ", ", ")")

  private inline def elemsList[L <: Tuple, T <: Tuple](v: Tuple): List[String] =
    val (head, tail) = v match
      case _: EmptyTuple => (None, None)
      case t: NonEmptyTuple => (Some(t.head), Some(t.tail))
    inline erasedValue[(L, T)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: (l *: ls, t *: ts) =>
        val label = constValue[l].asInstanceOf[String]
        val show = summonInline[Show[t]]
        val value = show.show(head.get.asInstanceOf[t])
        s"$label: $value" :: elemsList[ls, ts](tail.get)
