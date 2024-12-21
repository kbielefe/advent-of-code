package day21
import algorithms.AStar
import parse.{*, given}

case class State(output: String, d1: Char, d2: Char, n: Char):
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

  def moveD1(dir: Char): Set[State] =
    directional.get(d1 -> dir).map(d => Set(copy(d1 = d))).getOrElse(Set.empty)

  def moveD2(dir: Char): Set[State] =
    directional.get(d2 -> dir).map(d => Set(copy(d2 = d))).getOrElse(Set.empty)

  def moveN(dir: Char): Set[State] =
    numeric.get(n -> dir).map(n => Set(copy(n = n))).getOrElse(Set.empty)

  def selectD1: Set[State] =
    if d1 == 'A' then
      selectD2
    else
      moveD2(d1)

  def selectD2: Set[State] =
    if d2 == 'A' then
      selectN
    else
      moveN(d2)

  def selectN: Set[State] =
    Set(copy(output = n.toString + output))

  def getNeighbors: Set[State] =
    "<>^v".toSet.flatMap(moveD1) ++ selectD1

given Read[List[String]] = Read("\n")
object Puzzle extends runner.Day[List[String], Int, Int]:
  def part1(codes: List[String]): Int =
    val initial = State("", 'A', 'A', 'A')
    codes.map(code => astar(code).getMinCost(initial).get * code.take(3).toInt).sum

  def part2(codes: List[String]): Int =
    ???

  def astar(code: String): AStar[State, Int] = new AStar(
    _.output == code,
    _ => 0,
    (_, _) => 1,
    0,
    _.getNeighbors
  )
