import scala.io.Source

val input = Source.fromFile("input19.txt").getLines.zipWithIndex.flatMap{case (line, row) =>
  line.zipWithIndex.map{case (char, col) => (row, col) -> char}.filterNot{_._2 == ' '}
}.toMap

val forwardOffsets = Map(
  's' -> ( 1,  0),
  'n' -> (-1,  0),
  'e' -> ( 0,  1),
  'w' -> ( 0, -1)
)

val leftOffsets = Map(
  's' -> ( 0,  1),
  'n' -> ( 0, -1),
  'e' -> (-1,  0),
  'w' -> ( 1,  0)
)

val rightOffsets = Map(
  's' -> ( 0, -1),
  'n' -> ( 0,  1),
  'e' -> ( 1,  0),
  'w' -> (-1,  0)
)

val leftTurn = Map(
  's' -> 'e',
  'n' -> 'w',
  'e' -> 'n',
  'w' -> 's'
)

val rightTurn = Map(
  's' -> 'w',
  'n' -> 'e',
  'e' -> 's',
  'w' -> 'n'
)

def getSquare(current: (Int, Int), dir: Char)(offsets: Map[Char, (Int, Int)]): (Int, Int) = {
  val (x, y) = current
  val (offX, offY) = offsets(dir)
  (x + offX, y + offY)
}

val initial = (input.keySet.find(_._1 == 0).get, 's')

def infinitePath = Iterator.iterate(initial){case (currentSquare, dir) =>
  def square = getSquare(currentSquare, dir) _
  val forwardSquare = square(forwardOffsets)
  val    leftSquare = square(leftOffsets)
  val   rightSquare = square(rightOffsets)
  val left    = input.getOrElse(leftSquare,    ' ')
  val right   = input.getOrElse(rightSquare,   ' ')
  val forward = input.getOrElse(forwardSquare, ' ')
  val current = input(currentSquare)

  if (forward != ' ') {
    (forwardSquare, dir)
  } else if (left != ' ') {
    (leftSquare, leftTurn(dir))
  } else if (right != ' ') {
    (rightSquare, rightTurn(dir))
  } else {
    (currentSquare, 'd') // Done
  }
}

def path = infinitePath takeWhile {_._2 != 'd'}

val answer1 = path.map(x => input(x._1)).filter{x => x != '-' && x != '|' && x != '+'}.mkString
println(answer1)

val answer2 = path.size
println(answer2)
