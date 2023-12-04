package algorithms
import scala.annotation.tailrec

import cats.*
import cats.data.*
import cats.syntax.all.*

trait Tree[A]:
  def children(node: A): Iterator[A]

object Tree:
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
