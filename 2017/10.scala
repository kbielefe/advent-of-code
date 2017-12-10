import Math.min

val raw = "192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12"
val input  = raw.split(",").map(_.toInt).toVector
val input2 = raw.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)
val max = 255
val inputLength = max + 1

def reverse(in: Vector[Int], start: Int, length: Int): Vector[Int] = {
  val endIndex = min(inputLength, start + length)
  val wrapIndex = if (start + length > max) (start + length) % inputLength else 0
  val wrapped = in.slice(start, endIndex) ++ in.slice(0, wrapIndex)
  val reversed = wrapped.reverse
  val a = if (start + length > max) reversed.drop(inputLength - start) else Vector.empty[Int]
  val b = if (length < inputLength) in.slice(wrapIndex, start) else Vector.empty[Int]
  val c = reversed.take(inputLength - start)
  val d = if (start + length < inputLength) in.slice(start + length, inputLength) else Vector.empty[Int]
  a ++ b ++ c ++ d
}

val initial = (0, 0, (0 to max).toVector)

def processInput(input: Vector[Int])(in: (Int, Int, Vector[Int])) = input.foldLeft(in){case ((start, skipSize, in), length) =>
  ((start + skipSize + length) % inputLength, skipSize + 1, reverse(in, start, length))
}

val processed = processInput(input)(initial)._3

val answer1 = processed(0) * processed(1)
println(answer1)

val sparseHash = Stream.iterate(initial)(processInput(input2)).drop(64).head._3
val denseHash = sparseHash.grouped(16).map(_.reduceLeft(_ ^ _))

val answer2 = denseHash.map{x => f"${x}%02x"}.mkString("")
println(answer2)
