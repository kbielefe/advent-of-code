package algorithms

/** A tree that requires knowing previously visited nodes in order to eliminate loops.
 */
trait VTree[A]:
  def children(node: A, visited: Set[A]): Iterator[A]

  def depthFirstTraverse(node: A): Iterator[A] =
    def helper(a: A, visited: Set[A]): Iterator[A] =
      Iterator(a) ++ children(a, visited).flatMap(child => helper(child, visited + a))
    helper(node, Set.empty)

  def breadthFirstTraverse(node: A): Iterator[A] =
    def helper(queue: Iterator[(A, Set[A])]): Iterator[(A, Set[A])] =
      if queue.hasNext then
        val (q1, q2) = queue.duplicate
        q1 ++ helper(q2.flatMap((a, visited) => children(a, visited).map(_ -> (visited + a))))
      else
        Iterator.empty
    helper(Iterator((node, Set.empty))).map(_._1)

  def preOrder(node: A): Iterator[A] =
    depthFirstTraverse(node)

  def postOrder(node: A): Iterator[A] =
    def helper(a: A, visited: Set[A]): Iterator[A] =
      children(a, visited).flatMap(child => helper(child, visited + a)) ++ Iterator(a)
    helper(node, Set.empty)

  def dfs(node: A, p: A => Boolean): Option[A] =
    depthFirstTraverse(node).find(p)

  def bfs(node: A, p: A => Boolean): Option[A] =
    breadthFirstTraverse(node).find(p)

  def allReachable(node: A, goal: A => Boolean): Set[A] =
    depthFirstTraverse(node).filter(goal).toSet

  def pathCount(node: A, goal: A => Boolean): Int =
    depthFirstTraverse(node).count(goal)

  def allShortestPaths(node: A, goal: A => Boolean): Iterator[List[A]] =
    def helper(queue: Iterator[(A, Set[A], List[A])]): Iterator[(A, Set[A], List[A])] =
      if queue.hasNext then
        val (q1, q2) = queue.duplicate
        q1 ++ helper(q2.flatMap((a, visited, path) => children(a, visited).map(child =>  (child, (visited + a), a :: path))))
      else
        Iterator.empty
    val (q1, q2) = helper(Iterator((node, Set.empty, List.empty))).dropWhile(x => !goal(x._1)).duplicate
    if q1.hasNext then
      val shortest = q1.next._3.size
      q2.takeWhile(_._3.size == shortest).filter(x => goal(x._1)).map(x => x._1 :: x._3)
    else
      Iterator.empty
