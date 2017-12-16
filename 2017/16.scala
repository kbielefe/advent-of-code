import scala.io.Source

val input = Source.fromFile("input16.txt").mkString.split(",").map(_.trim)

def spin(amount: Int, order: String): String =
  order.takeRight(amount) ++ order.take(order.size - amount)

def exchange(positions: String, order: String): String = {
  val Array(x, y) = positions.split("/").map(_.toInt)
  order.updated(x, order(y)).updated(y, order(x))
}

def partner(programs: String, order: String): String = {
  val x = programs(0)
  val y = programs(2)
  order.updated(order.indexOf(x), y).updated(order.indexOf(y), x)
}

val initial = ('a' to 'p').mkString

def danceMoves(initial: String): String = input.foldLeft(initial){case (order, move) =>
  move.head match {
    case 's' => spin(move.tail.toInt, order)
    case 'x' => exchange(move.tail, order)
    case 'p' => partner(move.tail, order)
  }
}

val answer1 = danceMoves(initial)
println(answer1)

def iterations = Iterator.iterate(initial)(danceMoves)

def lengthUntilLoop = iterations.drop(1).takeWhile(_ != initial).size + 1

val answer2 = iterations.drop(1000000000 % lengthUntilLoop).next

println(answer2)
