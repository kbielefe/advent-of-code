import scala.io.Source

val input = Source.fromFile("input6.txt").getLines.map{_ split ", " map {_.toInt}}.map{x => (x(0), x(1))}.toList

type Point = (Int, Int)

def distance(a: Point, b: Point): Int = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

val minX = input.map{_._1}.min
val maxX = input.map{_._1}.max
val minY = input.map{_._2}.min
val maxY = input.map{_._2}.max

val pointsInsideBoundingBox = for {
  x <- minX to maxX
  y <- minY to maxY
} yield (x, y)

val pointsBorderingBoundingBox =
  (((minX - 1) to (maxX + 1)).map{x => List((x, minY - 1), (x, maxY + 1))} ++
  ((minY - 1) to (maxY + 1)).map{y => List((minX - 1, y), (maxX + 1, y))}).flatten

def nearestNeighbor(a: Point): Option[Point] = {
  val distances = input map {distance(a, _)}
  val minDistance = distances.min
  val closestPoints = distances.zip(input).filter{_._1 == minDistance}.map{_._2}
  if (closestPoints.size == 1)
    Some(closestPoints.head)
  else
    None
}

val infiniteRegions = pointsBorderingBoundingBox.map(nearestNeighbor).flatten.toSet
val regions = pointsInsideBoundingBox.map(nearestNeighbor).flatten.filterNot{infiniteRegions contains _}
val regionSizes = regions.groupBy(identity).mapValues{_.size}
val largestRegion = regionSizes.maxBy{_._2}

val answer1 = largestRegion._2
println(answer1)

def totalDistanceToAllCoordinates(a: Point): Int = input.map{distance(a, _)}.sum

val pointsWithinRegion = pointsInsideBoundingBox filter {totalDistanceToAllCoordinates(_) < 10000}
val answer2 = pointsWithinRegion.size

println(answer2)
