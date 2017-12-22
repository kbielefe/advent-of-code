import scala.io.Source

val rawInput = Source.fromFile("input22.txt").getLines.zipWithIndex.flatMap{case (line, row) =>
  line.zipWithIndex.filter{_._1 == '#'}.map{case (_, col) => (row, col)}
}.toSet

val rowOffset = rawInput.map(_._1).max / 2
val colOffset = rawInput.map(_._2).max / 2
val input = rawInput.map{case (row, col) => (row - rowOffset, col - colOffset)}

val right = Map(
  'n' -> 'e',
  'e' -> 's',
  's' -> 'w',
  'w' -> 'n'
)

val left = Map(
  'n' -> 'w',
  'e' -> 'n',
  's' -> 'e',
  'w' -> 's'
)

val moveOffsets = Map(
  'n' -> (-1,  0),
  's' -> ( 1,  0),
  'e' -> ( 0,  1),
  'w' -> ( 0, -1)
)

def moves = Iterator.iterate((input, (0, 0), 'n', false)){case (infections, pos@(row, col), dir, _) =>
  val infected = infections contains pos
  val newDir = if (infected) right(dir) else left(dir)
  val newInfections = if (infected) infections - pos else infections + pos
  val (rowOff, colOff) = moveOffsets(newDir)
  val newPos = (row + rowOff, col + colOff)
  (newInfections, newPos, newDir, !infected)
}

val answer1 = moves.drop(1).take(10000).count(_._4)
println(answer1)

def moves2 = Iterator.iterate((input, Set.empty[(Int, Int)], Set.empty[(Int, Int)], (0, 0), 'n', false)){case (infections, weakenedSquares, flaggedSquares, pos@(row, col), dir, _) =>
  val infected = infections contains pos
  val weakened = weakenedSquares contains pos
  val flagged = flaggedSquares contains pos
  val clean = !infected && !weakened && !flagged
  val newDir = if (clean) left(dir) else if (weakened) dir else if (flagged) left(left(dir)) else right(dir)
  val newInfections = if (weakened) infections + pos else infections - pos
  val newWeakened = if (clean) weakenedSquares + pos else weakenedSquares - pos
  val newFlagged = if (infected) flaggedSquares + pos else flaggedSquares - pos
  val (rowOff, colOff) = moveOffsets(newDir)
  val newPos = (row + rowOff, col + colOff)
  (newInfections, newWeakened, newFlagged, newPos, newDir, weakened)
}

val answer2 = moves2.drop(1).take(10000000).count(_._6)
println(answer2)
