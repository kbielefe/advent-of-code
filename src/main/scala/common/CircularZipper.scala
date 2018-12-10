package common
import scalaz.DList

case class CircularZipper[A](left: DList[A] = DList[A](), right: DList[A] = DList[A]()) {

  def isEmpty: Boolean = left.isEmpty && right.isEmpty

  def current: A = right.headOption.get

  def insert(elem: A): CircularZipper[A] = CircularZipper(elem +: left, elem +: right)

  def delete: CircularZipper[A] = CircularZipper(left.tailOption.get, right.tailOption.get)

  def moveRight: CircularZipper[A] = this

  def moveLeft: CircularZipper[A] = this
}
