package day21
import parse.{*, given}
import algorithms.VTree

val directional = Map(
  ('^', '>') -> 'A',
  ('^', 'v') -> 'v',
  ('A', '<') -> '^',
  ('A', 'v') -> '>',
  ('<', '>') -> 'v',
  ('v', '^') -> '^',
  ('v', '<') -> '<',
  ('v', '>') -> '>',
  ('>', '^') -> 'A',
  ('>', '<') -> 'v'
)

val numeric = Map(
  ('A', '<') -> '0',
  ('A', '^') -> '3',
  ('0', '>') -> 'A',
  ('0', '^') -> '2',
  ('1', '^') -> '4',
  ('1', '>') -> '2',
  ('2', '<') -> '1',
  ('2', '>') -> '3',
  ('2', '^') -> '5',
  ('2', 'v') -> '0',
  ('3', '<') -> '2',
  ('3', '^') -> '6',
  ('3', 'v') -> 'A',
  ('4', '^') -> '7',
  ('4', '>') -> '5',
  ('4', 'v') -> '1',
  ('5', '<') -> '4',
  ('5', '>') -> '6',
  ('5', '^') -> '8',
  ('5', 'v') -> '2',
  ('6', '<') -> '5',
  ('6', '^') -> '9',
  ('6', 'v') -> '3',
  ('7', 'v') -> '4',
  ('7', '>') -> '8',
  ('8', '<') -> '7',
  ('8', 'v') -> '5',
  ('8', '>') -> '9',
  ('9', '<') -> '8',
  ('9', 'v') -> '6'
)

given Read[List[String]] = Read("\n")
object Puzzle extends runner.Day[List[String], Int, Int]:
  def part1(codes: List[String]): Int =
    allShortestPaths(numeric)(codes.head.toList).foreach(println)
    codes.map(code => minCost(code.toList) * code.take(3).toInt).sum

  def part2(codes: List[String]): Int =
    ???

  def minCost(code: List[Char]): Int =
    val result = allShortestPaths(numeric)(code)
      .flatMap(allShortestPaths(directional))
      .flatMap(allShortestPaths(directional))
      .map(_.size)
      .min
    result

  def allShortestPaths(neighbors: Map[(Char, Char), Char])(code: List[Char]): Iterator[List[Char]] =
    concatPaths(('A' :: code).sliding(2).map{case List(l, r) => vtree(neighbors).allShortestPaths(l, _ == r)})

  def concatPaths(paths: Iterator[Iterator[List[Char]]]): Iterator[List[Char]] =
    def helper(prefix: List[Char], paths: Iterator[Iterator[List[Char]]]): Iterator[List[Char]] =
      if paths.isEmpty then
        Iterator(prefix)
      else
        paths.next.flatMap(path => helper(path ++ ('A' :: prefix.drop(1)), paths))
    helper(List.empty, paths)

  def vtree(neighbors: Map[(Char, Char), Char]): VTree[Char] = new VTree[Char]:
    override def children(node: Char, visited: Set[Char]): Iterator[Char] =
      "><^v".iterator.flatMap(dir => neighbors.get(node -> dir).filterNot(visited.contains))
