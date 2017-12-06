val input = "4  10  4 1 8 4 9 14  5 1 14  15  0 15  3 5".split("\\s+").map(_.toInt)

def redistribute(input: Array[Int]): Array[Int] = {
  val max = input.max
  val index = input.indexWhere(_ == max)
  val zeroed = input.updated(index, 0)
  (max to 1 by -1).foldLeft(zeroed){case (xs, offset) =>
    val rotI = (index + offset) % input.size
    xs.updated(rotI, xs(rotI) + 1)
  }
}
 
val redistributions = Stream.iterate(input)(redistribute)
val cumulative = redistributions.scanLeft(Set.empty[Array[Int]])(_ + _)
val zipped = redistributions zip cumulative
val results = zipped takeWhile {case (x, seen) => !seen.exists(_ sameElements x)}
val answer1 = results.size

println(answer1)

val looped = redistribute(results.last._1)
val beforeLoop = redistributions takeWhile {!_.sameElements(looped)}
val answer2 = answer1 - beforeLoop.size
println(answer2)
