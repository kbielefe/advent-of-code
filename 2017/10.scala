import Math.min

object advent10 {
  val raw = "192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12"
  val input  = raw.split(",").map(_.toInt).toVector
  val input2 = raw.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)
  val max = 255
  val inputLength = max + 1

  def reverse(in: Circular[Int], start: Int, length: Int): Circular[Int] = {
    val slice = in.slice(start, start + length)
    in.patch(start, slice.reverse, length)
  }

  val initial = (0, 0, new Circular((0 to max).toVector))

  def processInput(input: Vector[Int])(in: (Int, Int, Circular[Int])) = input.foldLeft(in){case ((start, skipSize, in), length) =>
    ((start + skipSize + length) % inputLength, skipSize + 1, reverse(in, start, length))
  }

  val processed = processInput(input)(initial)._3

  val answer1 = processed(0) * processed(1)

  val sparseHash = Stream.iterate(initial)(processInput(input2)).drop(64).head._3
  val denseHash = sparseHash.grouped(16).map(_.reduceLeft(_ ^ _))

  val answer2 = denseHash.map{x => f"${x}%02x"}.mkString("")

  def main(args: Array[String]): Unit = {
    println(answer1)
    println(answer2)
  }
}
