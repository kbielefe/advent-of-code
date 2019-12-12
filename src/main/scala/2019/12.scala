package advent2019
import common.DayTask
import common.Dynamic.detectCycle
import common.Numeric.gcd
import monix.eval.Task
import monix.reactive.Observable

class Day12 extends DayTask[List[(Int, Int, Int)], Int, String] {

  type Coord = (Int, Int, Int)

  override def input(lines: Observable[String]): Task[List[(Int, Int, Int)]] = Task{
    List((0, 6, 1), (4, 4, 19), (-11, 1, 8), (2, 19, 15))
  }

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

  def iterator(initialPositions: List[(Int, Int, Int)]) =
    Iterator.iterate((initialPositions, initialVelocities)){case (pos, vel) => timeStep(pos, vel)}

  def period(initialPositions: List[(Int, Int, Int)], f: ((Int, Int, Int)) => Int) = Task{
    detectCycle(iterator(initialPositions).map{case (pos, vel) => (pos.map(f), vel.map(f))}).get._2
  }

  override def part1(input: List[(Int, Int, Int)]) = Task{
    val (pos, vel) = iterator(input).drop(1000).next
    totalEnergy(pos, vel)
  }

  override def part2(input: List[(Int, Int, Int)]) = for {
    cx <- period(input, _._1).start
    cy <- period(input, _._2).start
    cz <- period(input, _._3).start
    result <- Task.gatherUnordered(Seq(cx.join, cy.join, cz.join))
  } yield s"LCM of ${result.mkString(" ")}"
}
