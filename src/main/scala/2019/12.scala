package advent2019
import common.Day
import common.Numeric.gcd
import common.Dynamic.detectCycle
import scala.io.Source

class Day12(source: Source) extends Day {

  type Coord = (Int, Int, Int)

  val samplePositions = List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1))
  val samplePositions2 = List((-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3))
  val initialPositions  = List((0, 6, 1), (4, 4, 19), (-11, 1, 8), (2, 19, 15))
  val initialVelocities = List((0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0))

  def applyGravity(positions: List[Coord], velocities: List[Coord]): List[Coord] = {
    val updates = positions.zipWithIndex.combinations(2).flatMap{case List(((x1, y1, z1), i1), ((x2, y2, z2), i2)) =>
      val x: List[(Coord, Int)] = if (x1 < x2) {
        List(((1, 0, 0), i1), ((-1, 0, 0), i2))
      } else if (x1 > x2) {
        List(((1, 0, 0), i2), ((-1, 0, 0), i1))
      } else List.empty
      val y: List[(Coord, Int)] = if (y1 < y2) {
        List(((0, 1, 0), i1), ((0, -1, 0), i2))
      } else if (y1 > y2) {
        List(((0, 1, 0), i2), ((0, -1, 0), i1))
      } else List.empty
      val z: List[(Coord, Int)] = if (z1 < z2) {
        List(((0, 0, 1), i1), ((0, 0, -1), i2))
      } else if (z1 > z2) {
        List(((0, 0, 1), i2), ((0, 0, -1), i1))
      } else List.empty
      x ++ y ++ z
    }
    updates.foldLeft(velocities){case (acc, ((x, y, z), i)) =>
      val (vx, vy, vz) = acc(i)
      acc.updated(i, (vx + x, vy + y, vz + z))
    }.toList
  }

  def applyVelocities(positions: List[Coord], velocities: List[Coord]): List[Coord] = {
    positions.zip(velocities).map{case ((px, py, pz), (vx, vy, vz)) => (px + vx, py + vy, pz + vz)}
  }

  def timeStep(positions: List[Coord], velocities: List[Coord]): (List[Coord], List[Coord]) = {
    val newVelocities = applyGravity(positions, velocities)
    val newPositions = applyVelocities(positions, newVelocities)
    (newPositions, newVelocities)
  }

  def totalEnergy(positions: List[Coord], velocities: List[Coord]): Int = {
    val potentialEnergies = positions.map{case (x, y, z) => math.abs(x) + math.abs(y) + math.abs(z)}
    val kineticEnergies = velocities.map{case (x, y, z) => math.abs(x) + math.abs(y) + math.abs(z)}
    potentialEnergies.zip(kineticEnergies).map{case (p, k) => p * k}.sum
  }

  def iterator = 
    Iterator.iterate((initialPositions, initialVelocities)){case (pos, vel) => timeStep(pos, vel)}

  def period(f: ((Int, Int, Int)) => Int) =
    detectCycle(iterator.map{case (pos, vel) => (pos.map(f), vel.map(f))}).get._2

  override def answer1: String = {
    val (pos, vel) = iterator.drop(1000).next
    totalEnergy(pos, vel).toString
  }

  override def answer2: String = {
    val cx = period(_._1)
    val cy = period(_._2)
    val cz = period(_._3)
    s"LCM of $cx $cy $cz"
  }
}
