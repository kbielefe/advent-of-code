package algorithms
import scala.annotation.tailrec

import cats.*
import cats.data.*
import cats.syntax.all.*

trait Tree[A]:
  def children(node: A): Iterator[A]

object Tree:
  def fromId[A, B](list: IterableOnce[A], id: A => B, neighborIds: A => IterableOnce[B]): Tree[A] = new Tree[A]:
    val nodeMap: Map[B, A] = list.iterator.map(node => id(node) -> node).toMap
    def children(node: A): Iterator[A] =
      neighborIds(node).iterator.map(nodeMap.apply)

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

    def depth(p: A => Boolean): Option[Int] =
      def helper(a: A, depth: Int): Eval[Option[Int]] =
        if p(a) then
          Eval.now(Some(depth))
        else
          a.children.toList.traverse(child => helper(child, depth + 1))
            .map(childrenDepth => childrenDepth.find(_.isDefined).flatten)
      helper(a, 0).value

    def allPathsTo(p: A => Boolean): Iterator[List[A]] =
      def helper(a: A, path: List[A]): Iterator[Eval[List[A]]] =
        Iterator(Eval.now(path)) ++ a.children.flatMap(child => helper(child, child :: path))
      helper(a, List(a)).sequence.value.filter(x => p(x.head))

