package algorithms

import math.Numeric.Implicits.infixNumericOps
import scala.compiletime.error
import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import scala.reflect.{Typeable, TypeTest}

sealed class Matrix[R <: Int, C <: Int, A : Numeric](val rows: Vector[Vector[A]])(using CanEqual[A, A]) derives CanEqual:
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

  def *[C2 <: Int](other: Matrix[C, C2, A]): Matrix[R, C2, A] =
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

object Matrix:
  /** Given a dynamically constructed set of rows whose size is known at compile time, construct a Matrix. */
  inline def apply[R <: Int : ValueOf, C <: Int : ValueOf, A : Numeric](uncheckedRows: IterableOnce[IterableOnce[A]])(using CanEqual[A, A]): Either[MatrixError, Matrix[R, C, A]] =
    inline if valueOf[<=[R, 0]] then error("A Matrix must have at least one row.")
    inline if valueOf[<=[C, 0]] then error("A Matrix must have at least one column.")

    val rows = uncheckedRows.map(_.toVector).toVector
    val r = valueOf[R]
    val c = valueOf[C]

    if r != rows.size then
      Left(MatrixError.MismatchedRows(r, rows.size))
    else if rows.exists(_.size != c) then
      Left(MatrixError.MismatchedCols(c, rows.map(_.size).find(_ != c).get))
    else
      Right(new Matrix(rows))

  inline def apply[B <: NonEmptyTuple, A: Numeric](rows: B*): Matrix[3, 3, A] = ???
enum MatrixError derives CanEqual:
  case MismatchedRows(expected: Int, actual: Int)
  case MismatchedCols(expected: Int, actual: Int)
