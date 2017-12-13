import scala.io.Source

val input = Source.fromFile("input5.txt").getLines.map(_.toInt).toVector

def results(f: Int => Int) = Stream.iterate((0, 0, input)){case (step, index, input) =>
  val offset = input(index)
  val newInput = input.updated(index, f(offset))
  (step + 1, index + offset, newInput)
}

def answer(f: Int => Int) = results(f).dropWhile(x => x._2 >= 0 && x._2 < input.size).head._1

println(answer(_ + 1))
println(answer(offset => if (offset >= 3) offset - 1 else offset + 1))
