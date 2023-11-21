package algorithms

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
