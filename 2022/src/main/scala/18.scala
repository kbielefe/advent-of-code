package advent2022
import algorithms.floodFill

object Day18:
  def part1(input: Set[(Int, Int, Int)]): Int =
    input.toList.map{case (x, y, z) =>
      Set(
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1)
      ).count(!input.contains(_))
    }.sum

  def part2(input: Set[(Int, Int, Int)]): Int =
    val minX = input.map(_._1).min - 1
    val minY = input.map(_._2).min - 1
    val minZ = input.map(_._3).min - 1
    val maxX = input.map(_._1).max + 1
    val maxY = input.map(_._2).max + 1
    val maxZ = input.map(_._3).max + 1
    def getNeighbors(cube: (Int, Int, Int)): Set[(Int, Int, Int)] =
      val (x, y, z) = cube
      Set(
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1)
      ).filter{case (x, y, z) => x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ && !input.contains((x, y, z))}
    val externalCubes = floodFill((minX, minY, minZ), getNeighbors)
    input.toList.map{case (x, y, z) =>
      Set(
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1)
      ).count(externalCubes.contains)
    }.sum
