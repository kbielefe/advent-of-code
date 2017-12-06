val input = "4  10  4 1 8 4 9 14  5 1 14  15  0 15  3 5".split("\\s+").map(_.toInt).toVector

def redistribute(input: Vector[Int]): Vector[Int] = {
  val max = input.max
  val index = input.indexWhere(_ == max)
  val zeroed = input.updated(index, 0)
  (1 to max).foldLeft(zeroed){case (xs, offset) =>
    val rotI = (index + offset) % input.size
    xs.updated(rotI, xs(rotI) + 1)
  }
}

val redistributions = Stream.iterate(input)(redistribute)
val cumulative = redistributions.scanLeft(Set.empty[Vector[Int]])(_ + _)
val zipped = redistributions zip cumulative
val results = zipped takeWhile {case (x, seen) => !seen.contains(x)}
val answer1 = results.size

println(answer1)

val looped = redistribute(results.last._1)
val beforeLoop = redistributions takeWhile {_ != looped}
val answer2 = answer1 - beforeLoop.size
println(answer2)
