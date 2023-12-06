package algorithms

import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec
import scala.compiletime.error
import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import scala.reflect.{Typeable, TypeTest}

sealed class Matrix[R <: Int : ValueOf, C <: Int : ValueOf, A : Numeric](val rows: Vector[Vector[A]])(using CanEqual[A, A]) derives CanEqual:
  private val r = rows.size
  private val c = rows.head.size

  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case o: Matrix[R, C, A] @unchecked => rows == o.rows
    case _ => false

  override def toString: String =
    val padding = rows.flatten.map(_.toString.size).max
    def pad(n: A) = s"%${padding}d".format(n)
    val width = padding * c + c - 1
    "\n┌ " + (" " * width) + " ┐\n" +
    rows.map(_.map(pad).mkString("│ ", " ", " │")).mkString("\n") +
    "\n└ " + (" " * width) + " ┘"

  def *[C2 <: Int : ValueOf](other: Matrix[C, C2, A]): Matrix[R, C2, A] =
    val cols = other.rows.transpose
    val resultRows = rows.map(row =>
      cols.map(col =>
        row.zip(col).map(_ * _).sum
      )
    )
    new Matrix(resultRows)

  def transpose: Matrix[C, R, A] = new Matrix(rows.transpose)

  def elements: Vector[A] = rows.flatten

  inline def row[R2 <: Int : ValueOf]: Matrix[1, C, A] =
    inline if valueOf[||[<[R2, 0],>=[R2, R]]] then error("Row number is out of range")
    new Matrix[1, C, A](Vector(rows(valueOf[R2])))

  inline def col[C2 <: Int : ValueOf]: Matrix[R, 1, A] =
    inline if valueOf[||[<[C2, 0],>=[C2, C]]] then error("Column number is out of range")
    new Matrix[R, 1, A](Vector(rows.transpose.apply(valueOf[C2])).transpose)

  inline def element[R2 <: Int : ValueOf, C2 <: Int : ValueOf]: A =
    inline if valueOf[||[<[R2, 0],>=[R2, R]]] then error("Row number is out of range")
    inline if valueOf[||[<[C2, 0],>=[C2, C]]] then error("Column number is out of range")
    rows(valueOf[R2])(valueOf[C2])

  def pow[N](e: N)(using R =:= C)(using CanEqual[N, N])(using n: Integral[N]): Matrix[C, C, A] =
    assert(e >= n.zero, "power must be greater than or equal to 0")
    @tailrec
    def helper(e: N, x: Matrix[C, C, A], result: Matrix[C, C, A]): Matrix[C, C, A] =
      if e == n.zero then
        result
      else if n.rem(e, n.fromInt(2)) == n.one then
        helper(e - n.one, x, x * result)
      else
        helper(n.quot(e, n.fromInt(2)), x * x, result)
    helper(e, this.asInstanceOf[Matrix[C, C, A]], Matrix.identity[C, A])

object Matrix:
  def apply[R <: Int : ValueOf, C <: Int : ValueOf, A : Numeric](uncheckedRows: IterableOnce[A]*)(using CanEqual[A, A]): Matrix[R, C, A] =
    val r = valueOf[R]
    val c = valueOf[C]
    assert(r > 0, "A matrix must have at least one row.")
    assert(c > 0, "A matrix must have at least one column.")

    val rows = uncheckedRows.iterator.map(_.iterator.to(Vector)).toVector

    if r != rows.size then
      throw MismatchedRows(r, rows.size)
    else if rows.exists(_.size != c) then
      throw MismatchedCols(c, rows.map(_.size).find(_ != c).get)
    else
      new Matrix(rows)

  def colVector[D <: Int : ValueOf, A : Numeric](elements: A*)(using CanEqual[A, A]): Matrix[D, 1, A] =
    apply[D, 1, A](List(elements).transpose*)

  def rowVector[D <: Int : ValueOf, A : Numeric](elements: A*)(using CanEqual[A, A]): Matrix[1, D, A] =
    apply[1, D, A](List(elements)*)

  def identity[D <: Int : ValueOf, A](using CanEqual[A, A])(using n: Numeric[A]): Matrix[D, D, A] =
    val d = valueOf[D]
    val rows = (1 to d).map{row =>
      (1 to d).map{col =>
        if row == col then n.one else n.zero
      }.toVector
    }.toVector

    new Matrix[D, D, A](rows)

  def translate[A](x: A, y: A)(using n: Numeric[A])(using CanEqual[A, A]): Matrix[3, 3, A] =
    apply[3, 3, A](List(n.one, n.zero, x), List(n.zero, n.one, y), List(n.zero, n.zero, n.one))

  def rotateCCW[A](using n: Numeric[A])(using CanEqual[A, A]): Matrix[3, 3, A] =
    apply[3, 3, A](List(n.zero, -n.one, n.zero), List(n.one, n.zero, n.zero), List(n.zero, n.zero, n.one))

  def rotateCW[A](using n: Numeric[A])(using CanEqual[A, A]): Matrix[3, 3, A] =
    apply[3, 3, A](List(n.zero, n.one, n.zero), List(-n.one, n.zero, n.zero), List(n.zero, n.zero, n.one))

  def reflectX[A](using n: Numeric[A])(using CanEqual[A, A]): Matrix[3, 3, A] =
    apply[3, 3, A](List(n.one, n.zero, n.zero), List(n.zero, -n.one, n.zero), List(n.zero, n.zero, n.one))

  def reflectY[A](using n: Numeric[A])(using CanEqual[A, A]): Matrix[3, 3, A] =
    apply[3, 3, A](List(-n.one, n.zero, n.zero), List(n.zero, n.one, n.zero), List(n.zero, n.zero, n.one))

  case class MismatchedRows(expected: Int, actual: Int) extends Exception(s"Expected $expected rows, found $actual")
  case class MismatchedCols(expected: Int, actual: Int) extends Exception(s"Expected $expected columns, found $actual")
