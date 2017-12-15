def generator(start: Int, factor: Int): Iterator[Long] =
  Iterator.iterate(start.toLong)(_ * factor % 2147483647) drop 1 map (_ & 0xffff)

def countEqual(a: Iterator[Long], b: Iterator[Long], take: Int): Int =
  (a zip b).take(take).count{case (x, y) => x == y}

def a = generator(116, 16807)
def b = generator(299, 48271)

val answer1 = countEqual(a, b, 40000000)
println(answer1)

val answer2 = countEqual(a filter (_ % 4 == 0), b filter (_ % 8 == 0), 5000000)
println(answer2)
