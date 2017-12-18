val input = 344

def insertPositions(max: Int) = (1 to max).iterator.scanLeft(0){case (pos, size) => (pos + input) % size + 1}

def lastVector(max: Int) = ((0 to max).iterator zip insertPositions(max)).foldLeft(Vector.empty[Int]){case (v, (elem, pos)) =>
  v.patch(pos, Vector(elem), 0)
}

def trackPosition(max: Int) =
    ((0 to max).iterator zip insertPositions(max)).foldLeft((0, -1)){case ((trackedPos, elemAfterTracked), (elem, pos)) =>
  val newTrackedPos = if (pos < trackedPos) trackedPos + 1 else trackedPos
  val newElemAfterTracked = if (pos == trackedPos + 1) elem else elemAfterTracked
  (newTrackedPos, newElemAfterTracked)
}

val answer1 = lastVector(2017).dropWhile(_ != 2017).drop(1).head
println(answer1)

val answer2 = trackPosition(50000000)._2
println(answer2)