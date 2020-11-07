package advent2019
import common.DayTask
import common.Dynamic.detectCycle
import common.Numeric.gcd
import monix.eval.Task
import monix.reactive.Observable

class Day12 extends DayTask[Vector[Vector[Int]], Int, String] {

  override def input(lines: Observable[String]): Task[Vector[Vector[Int]]] = Task{
    Vector(Vector(0, 4, -11, 2), Vector(6, 4, 1, 19), Vector(1, 19, 8, 15))
  }

  val initialVelocities = Vector(0, 0, 0, 0)

  def applyGravity(positions: Vector[Int], velocities: Vector[Int]): Vector[Int] = {
    val updates = positions.zipWithIndex.combinations(2).flatMap{case Vector((x1, i1), (x2, i2)) =>
      if (x1 < x2) {
        Vector((1, i1), (-1, i2))
      } else if (x1 > x2) {
        Vector((1, i2), (-1, i1))
      } else Vector.empty
    }
    updates.foldLeft(velocities){case (acc, (x, i)) =>
      val vx = acc(i)
      acc.updated(i, acc(i) + x)
    }.toVector
  }

  def applyVelocities(positions: Vector[Int], velocities: Vector[Int]): Vector[Int] = {
    positions.zip(velocities).map{case (px, vx) => px + vx}
  }

  def timeStep(positions: Vector[Int], velocities: Vector[Int]): (Vector[Int], Vector[Int]) = {
    val newVelocities = applyGravity(positions, velocities)
    val newPositions = applyVelocities(positions, newVelocities)
    (newPositions, newVelocities)
  }

  def totalEnergy(positions: Vector[Vector[Int]], velocities: Vector[Vector[Int]]): Int = {
    val potentialEnergies = positions.transpose.map{case Vector(x, y, z) => math.abs(x) + math.abs(y) + math.abs(z)}
    val kineticEnergies = velocities.transpose.map{case Vector(x, y, z) => math.abs(x) + math.abs(y) + math.abs(z)}
    potentialEnergies.zip(kineticEnergies).map{case (p, k) => p * k}.sum
  }

  def iterator(initialPositions: Vector[Int]) =
    Iterator.iterate((initialPositions, initialVelocities)){case (pos, vel) => timeStep(pos, vel)}

  def period(initialPositions: Vector[Int]) = Task{
    iterator(initialPositions)
      .drop(1)
      .takeWhile{case (pos, vel) => pos != initialPositions || vel != initialVelocities}
      .size + 1
  }

  override def part1(input: Vector[Vector[Int]]) = Task{
    val (pos1, vel1) = iterator(input(0)).drop(1000).next()
    val (pos2, vel2) = iterator(input(1)).drop(1000).next()
    val (pos3, vel3) = iterator(input(2)).drop(1000).next()
    totalEnergy(Vector(pos1, pos2, pos3), Vector(vel1, vel2, vel3))
  }

  // 282270365571288
  override def part2(input: Vector[Vector[Int]]) = for {
    cx <- period(input(0)).start
    cy <- period(input(1)).start
    cz <- period(input(2)).start
    result <- Task.parSequenceUnordered(Seq(cx.join, cy.join, cz.join))
  } yield s"LCM of ${result.mkString(" ")}"
}
