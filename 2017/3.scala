import Math.abs

val input = 325489

type Square = (Int, Int)

val offset = Map('n' -> (0,1), 'e' -> (1,0), 's' -> (0,-1), 'w' -> (-1,0))
val initialExtents = Map('n' -> 0, 'e' -> 0, 's' -> 0, 'w' -> 0)
val initial = ('e', 1, initialExtents, (0, 0), Map((0,0) -> 1))

def spiral(f: (Int, Square, Map[Square, Int]) => Int) = Stream.iterate(initial){case (dir, value, extents, (x, y), squares) =>
  val (xOff, yOff) = offset(dir)
  val newSquare@(newX, newY) = (x + xOff, y + yOff)

  def newVals(value: Int, f: (Int, Int) => Boolean, newDir: Char) =
    if (f(value, extents(dir))) (extents + (dir -> value), newDir) else (extents, dir)

  val (newExtents, newDir) = dir match {
    case 'n' => newVals(newY, _ > _, 'w')
    case 'e' => newVals(newX, _ > _, 'n')
    case 's' => newVals(newY, _ < _, 'e')
    case 'w' => newVals(newX, _ < _, 's')
  }

  val newValue = f(value, newSquare, squares)

  (newDir, newValue, newExtents, newSquare, squares + (newSquare -> newValue))
}

val increment = (value: Int, _: (Int, Int), _: Map[Square, Int]) => value + 1

val sumOfNeighbors = (value: Int, square: (Int, Int), squares: Map[Square, Int]) => {
  val (x,y) = square
  val neighbors = for {
    neighborX <- x - 1 to x + 1
    neighborY <- y - 1 to y + 1
  } yield squares.getOrElse((neighborX, neighborY), 0)
  neighbors.sum
}

val answer1 = spiral(increment).dropWhile(_._2 < input).map(_._4).map{case (x,y) => abs(x)+abs(y)}.head
println(answer1)
val answer2 = spiral(sumOfNeighbors).map(_._2).dropWhile(_ <= input).head
println(answer2)

