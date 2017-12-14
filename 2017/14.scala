import Math.{min, max}

val input = "jxqlasbh"
val testInput = "flqrgnkx"
val max = 255
val inputLength = max + 1

def reverse(in: Circular[Int], start: Int, length: Int): Circular[Int] = {
  val slice = in.slice(start, start + length)
  in.patch(start, slice.reverse, length)
}

def processInput(input: Vector[Int])(in: (Int, Int, Circular[Int])) = input.foldLeft(in){case ((start, skipSize, in), length) =>
  ((start + skipSize + length) % inputLength, skipSize + 1, reverse(in, start, length))
}

def knotHash(raw: String): String = {
  val input = raw.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)
  val initial = (0, 0, new Circular((0 to max).toVector))
  val sparseHash = Stream.iterate(initial)(processInput(input)).drop(64).head._3
  val denseHash = sparseHash.grouped(16).map(_.reduceLeft(_ ^ _))
  denseHash.map{x => f"${x.toBinaryString.toInt}%08d"}.mkString
}

val squares = (0 to 127)
  .flatMap(row => knotHash(input + "-" + row)
    .zipWithIndex
    .filter(_._1 == '1')
    .map(x => (row, x._2))
  ).toSet

val answer1 = squares.size
println(answer1)

def region(head: (Int, Int), result: Set[(Int, Int)] = Set.empty[(Int, Int)]): Set[(Int, Int)] = {
  val (row, col) = head
  val neighbors = (Set(
    (row + 1, col),
    (row - 1, col),
    (row, col + 1),
    (row, col - 1),
    (row, col)
  ) & squares) -- result
  if (neighbors.isEmpty)
    result
  else {
    val newResult = result ++ neighbors
    neighbors.flatMap(neighbor => region(neighbor, newResult))
  }
}

@scala.annotation.tailrec
def regionCount(squares: Set[(Int, Int)], count: Int = 0): Int = {
  if (squares.isEmpty) {
    count
  } else {
    val head = squares.head
    val currentRegion = region(head)
    println(currentRegion)
    val tail = squares -- currentRegion
    regionCount(tail, count + 1)
  }
}

val answer2 = regionCount(squares)
println(answer2)
