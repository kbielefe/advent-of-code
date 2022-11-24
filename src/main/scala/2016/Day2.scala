package advent2016

object Day2:
  def part1(input: List[String]): String =
    solve(input, part1Map)

  def part2(input: List[String]): String =
    solve(input, part2Map)

  def solve(input: List[String], map: Map[(Int, Int), Char]): String =
    val start = map.find(_._2 == '5').get._1
    val positions = input.scanLeft(start){case (pos, moves) =>
      moves.foldLeft(pos){case (pos, move) => next(pos, move, map)}
    }.drop(1)
    positions.map(map.apply).mkString

  private val part1Map = Map(
    (0, 0) -> '1',
    (1, 0) -> '2',
    (2, 0) -> '3',
    (0, 1) -> '4',
    (1, 1) -> '5',
    (2, 1) -> '6',
    (0, 2) -> '7',
    (1, 2) -> '8',
    (2, 2) -> '9'
  )

  private val part2Map = Map(
    (2, 0) -> '1',
    (1, 1) -> '2',
    (2, 1) -> '3',
    (3, 1) -> '4',
    (0, 2) -> '5',
    (1, 2) -> '6',
    (2, 2) -> '7',
    (3, 2) -> '8',
    (4, 2) -> '9',
    (1, 3) -> 'A',
    (2, 3) -> 'B',
    (3, 3) -> 'C',
    (2, 4) -> 'D'
  )

  private def next(pos: (Int, Int), move: Char, map: Map[(Int, Int), Char]): (Int, Int) =
    val (x, y) = pos
    val potential = move match
      case 'U' => (x, y - 1)
      case 'D' => (x, y + 1)
      case 'L' => (x - 1, y)
      case 'R' => (x + 1, y)
    if map.contains(potential) then potential else pos
