import scala.io.Source

val lines = Source.fromFile("input12.txt").getLines
val input = lines.map(_ split " <-> ").map{x => (x(0).toInt, x(1).split(", ").map(_.trim.toInt).toSet)}.toMap

def zipFunc[A,B](left: Stream[A], right: Stream[B])(f: Stream[B] => Stream[B]): Stream[A] = {
}

def group(root: Int): Set[Int] = {
  val closure = Stream.iterate(Set(root)){_ flatMap {x => input(x) + x}}
  val sizes = closure map {_.size}
  val sizesEqual = (sizes zip (sizes drop 1)).map{case (x, y) => x == y}
  (closure zip sizesEqual).dropWhile(!_._2).map(_._1).head
}

val answer1 = group(0).size
println(answer1)

val answer2 = input.keySet.map(group).size
println(answer2)
