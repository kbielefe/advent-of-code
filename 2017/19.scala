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

def getSquare(x: Int, y: Int, dir: Char, offsets: Map[Char, (Int, Int)]): (Int, Int) = {
  val (offX, offY) = offsets(dir)
  (x + offX, y + offY)
}

val initial = (input.keySet.find(_._1 == 0).get, 's')

def infinitePath = Iterator.iterate(initial){case ((x, y), dir) =>
  val forwardSquare = getSquare(x, y, dir, forwardOffsets)
  val    leftSquare = getSquare(x, y, dir, leftOffsets)
  val   rightSquare = getSquare(x, y, dir, rightOffsets)
  val   forwardChar = if (dir == 'n' || dir == 's') '|' else '-'
  val      turnChar = if (dir == 'n' || dir == 's') '-' else '|'
  val left    = input.getOrElse(leftSquare,    ' ')
  val right   = input.getOrElse(rightSquare,   ' ')
  val forward = input.getOrElse(forwardSquare, ' ')
  val current = input.getOrElse((x, y), ' ')

  if (current != '+' && forward != ' ') {
    (forwardSquare, dir)
  } else if (current == '+' && forward != ' ' && forward != turnChar) {
    (forwardSquare, dir)
  } else if (current == '+' && left != ' ' && left != forwardChar) {
    (leftSquare, leftTurn(dir))
  } else if (current == '+' && right != ' ' && right != forwardChar){
    (rightSquare, rightTurn(dir))
  } else {
    ((x, y), 'd') // Done
  }
}

def path = infinitePath takeWhile {_._2 != 'd'}

val answer1 = path.map(x => input(x._1)).filter{x => x != '-' && x != '|' && x != '+'}.mkString
println(answer1)

val answer2 = path.size
println(answer2)
