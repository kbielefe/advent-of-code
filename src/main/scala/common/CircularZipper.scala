package common

case class CircularZipper[A](left: List[A] = List.empty[A], right: List[A] = List.empty[A]) {

  def isEmpty: Boolean = left.isEmpty && right.isEmpty

  def current: A = left.head

  def insertRight(elem: A): CircularZipper[A] = CircularZipper(elem :: left, right)

  def delete: CircularZipper[A] = CircularZipper(left.tail, right).ensureCurrentOnLeft()

  def moveRight: CircularZipper[A] = {
    if (left.isEmpty && right.isEmpty) {
      this
    } else if (!left.isEmpty && right.isEmpty) {
      val reversedLeft = left.reverse
      CircularZipper(reversedLeft.head :: Nil, reversedLeft.tail)
    } else {
      CircularZipper(right.head :: left, right.tail)
    }
  }

  def moveRightN(n: Int): CircularZipper[A] = {
    Iterator.iterate(this){_.moveRight}.drop(n).next()
  }

  def moveLeftN(n: Int): CircularZipper[A] = {
    Iterator.iterate(this){_.moveLeft}.drop(n).next()
  }

  private def ensureCurrentOnLeft(): CircularZipper[A] = {
    if (left.isEmpty && !right.isEmpty) {
      CircularZipper(right.reverse, Nil)
    } else {
      this
    }
  }

  def moveLeft: CircularZipper[A] = {
    if (left.isEmpty && right.isEmpty) {
      this
    } else if (left.isEmpty && !right.isEmpty) {
      val reversedRight = right.reverse
      CircularZipper(reversedRight.tail, reversedRight.head :: Nil).ensureCurrentOnLeft()
    } else {
      CircularZipper(left.tail, left.head :: right).ensureCurrentOnLeft()
    }
  }
}
