package algorithms

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.{Typeable, TypeTest}

trait Tree[A]:
  def children(node: A): Iterator[A]

extension [A](a: A)(using t: Tree[A])
  def children: Iterator[A] = t.children(a)

  def depthFirstTraverse: Iterator[A] =
    Iterator(a) ++ children.flatMap(_.depthFirstTraverse)

  def breadthFirstTraverse: Iterator[A] =
    def helper(queue: Iterator[A]): Iterator[A] =
      if queue.hasNext then
        val (q1, q2) = queue.duplicate
        q1 ++ helper(q2.flatMap(t.children))
      else
        Iterator.empty
    helper(Iterator(a))

  def dfs(p: A => Boolean): Option[A] =
    depthFirstTraverse.find(p)

  def bfs(p: A => Boolean): Option[A] =
    breadthFirstTraverse.find(p)

object Tree:
  inline given derived[A](using m: Mirror.Of[A]): Tree[A] =
    inline m match
      case s: Mirror.SumOf[A]     => deriveSum(s)
      case p: Mirror.ProductOf[A] => deriveProduct(using p)

  private inline def deriveSum[A](m: Mirror.SumOf[A]): Tree[A] =
    new Tree[A]:
      def children(node: A): Iterator[A] =
        sumChildren[m.MirroredElemTypes, A](m, node, 0)

  private inline def deriveProduct[A](using Mirror.ProductOf[A]): Tree[A] =
    new Tree[A]:
      def children(node: A): Iterator[A] =
        productIterator[A, A](node)

  private inline def sumChildren[Elem <: Tuple, A](m: Mirror.SumOf[A], node: A, ordinal: Int): Iterator[A] =
    inline erasedValue[Elem] match
      case _: EmptyTuple => Iterator.empty
      case _: (Product *: t) =>
        if m.ordinal(node) == ordinal then
          inline erasedValue[Elem] match
            case _: (h *: t) =>
              productIterator[A, h](node.asInstanceOf[h]).asInstanceOf[Iterator[A]] ++ sumChildren[t, A](m, node, ordinal + 1)
        else
          sumChildren[t, A](m, node, ordinal + 1)
      case _: (h *: t) => sumChildren[t, A](m, node, ordinal + 1)

  private inline def productIterator[A, B](node: B): Iterator[A] =
    node.asInstanceOf[Product].productIterator.map(_.asInstanceOf[Matchable]).flatMap{
      case child: A                             => Iterator(child)
      case children: IterableOnce[A] @unchecked => children.iterator
      case _                                    => Iterator.empty
    }
