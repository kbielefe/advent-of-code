package advent2016

object Day1:
  def part1(input: String): Int =
    ???

  def part2(input: String): Int =
    val moves = input.split(", ").map(x => (x.head, x.tail.trim.toInt)).toList
    val (path1, path2) = pathFromMoves('N', (0, 0), moves).duplicate
    val visited = path1.scanLeft(Set.empty[(Int, Int)])(_ + _)
    val (_, (x, y)) = visited.zip(path2).dropWhile(!_.contains(_)).next
    Math.abs(x) + Math.abs(y)

  def pathFromMoves(dir: Char, pos: (Int, Int), moves: List[(Char, Int)]): Iterator[(Int, Int)] =
    if moves.isEmpty then
      Iterator.empty
    else
      val (x, y) = pos
      val (turn, distance) :: tail = moves: @unchecked
      val newDir = (dir, turn) match
        case ('N', 'R') => 'E'
        case ('E', 'R') => 'S'
        case ('S', 'R') => 'W'
        case ('W', 'R') => 'N'
        case ('N', 'L') => 'W'
        case ('E', 'L') => 'N'
        case ('S', 'L') => 'E'
        case ('W', 'L') => 'S'

      newDir match
        case 'N' => (1 to distance).iterator.map(d => (x, y + d)) ++ pathFromMoves(newDir, (x, y + distance), tail)
        case 'S' => (1 to distance).iterator.map(d => (x, y - d)) ++ pathFromMoves(newDir, (x, y - distance), tail)
        case 'E' => (1 to distance).iterator.map(d => (x + d, y)) ++ pathFromMoves(newDir, (x + distance, y), tail)
        case 'W' => (1 to distance).iterator.map(d => (x - d, y)) ++ pathFromMoves(newDir, (x - distance, y), tail)

