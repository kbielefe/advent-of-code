package advent2020

import common._

object Day17 extends SyncGridDay[Int, Int](2020, 17) {
  override def part1(input: Map[(Int, Int), Char]): Int = {
    val input3d = input.map{case ((x, y), char) => ((x, y, 0), char)}.toMap
    Iterator.iterate(input3d)(conwayCube).drop(6).next().map(_._2).count(_ == '#')
  }

  override def part2(input: Map[(Int, Int), Char]): Int = {
    val input4d = input.map{case ((x, y), char) => ((x, y, 0, 0), char)}.toMap
    Iterator.iterate(input4d)(conwayHyperCube).drop(6).next().map(_._2).count(_ == '#')
  }

  private def conwayCube(input: Map[(Int, Int, Int), Char]): Map[(Int, Int, Int), Char] = {
    val extended = extendMap(input)
    extended.map{case (point, char) =>
      val activeNeighbors = neighbors(point).toList.map(neighbor => extended.getOrElse(neighbor, '.')).count(_ == '#')
      val newChar =
        if (char == '#' && (activeNeighbors == 2 || activeNeighbors == 3))
          '#'
        else if (char == '.' && activeNeighbors == 3)
          '#'
        else
          '.'
      (point, newChar)
    }
  }

  private def extendMap(input: Map[(Int, Int, Int), Char]): Map[(Int, Int, Int), Char] = {
    val xs = input.keySet.map(_._1)
    val ys = input.keySet.map(_._2)
    val zs = input.keySet.map(_._3)
    val points = for {
      x <- (xs.min - 1) to (xs.max + 1)
      y <- (ys.min - 1) to (ys.max + 1)
      z <- (zs.min - 1) to (zs.max + 1)
    } yield (x, y, z)
    points.map(point => (point, input.getOrElse(point, '.'))).toMap
  }

  private def neighbors(point: (Int, Int, Int)): Set[(Int, Int, Int)] = {
    val (x, y, z) = point
    val ranges = for {
      nx <- (x - 1) to (x + 1)
      ny <- (y - 1) to (y + 1)
      nz <- (z - 1) to (z + 1)
    } yield (nx, ny, nz)
    ranges.toSet - point
  }

  private def conwayHyperCube(input: Map[(Int, Int, Int, Int), Char]): Map[(Int, Int, Int, Int), Char] = {
    val extended = hyperExtendMap(input)
    extended.map{case (point, char) =>
      val activeNeighbors = hyperNeighbors(point).toList.map(neighbor => extended.getOrElse(neighbor, '.')).count(_ == '#')
      val newChar =
        if (char == '#' && (activeNeighbors == 2 || activeNeighbors == 3))
          '#'
        else if (char == '.' && activeNeighbors == 3)
          '#'
        else
          '.'
      (point, newChar)
    }
  }

  private def hyperExtendMap(input: Map[(Int, Int, Int, Int), Char]): Map[(Int, Int, Int, Int), Char] = {
    val ws = input.keySet.map(_._1)
    val xs = input.keySet.map(_._2)
    val ys = input.keySet.map(_._3)
    val zs = input.keySet.map(_._4)
    val points = for {
      w <- (ws.min - 1) to (ws.max + 1)
      x <- (xs.min - 1) to (xs.max + 1)
      y <- (ys.min - 1) to (ys.max + 1)
      z <- (zs.min - 1) to (zs.max + 1)
    } yield (w, x, y, z)
    points.map(point => (point, input.getOrElse(point, '.'))).toMap
  }

  private def hyperNeighbors(point: (Int, Int, Int, Int)): Set[(Int, Int, Int, Int)] = {
    val (w, x, y, z) = point
    val ranges = for {
      nw <- (w - 1) to (w + 1)
      nx <- (x - 1) to (x + 1)
      ny <- (y - 1) to (y + 1)
      nz <- (z - 1) to (z + 1)
    } yield (nw, nx, ny, nz)
    ranges.toSet - point
  }

  private def cubeString(cube: Map[(Int, Int, Int), Char]): String = {
    val zs = cube.keySet.map(_._3)
    (zs.min to zs.max).map{z => layerString(z, cube)}.mkString(s"\n\n")
  }

  private def layerString(z: Int, cube: Map[(Int, Int, Int), Char]): String = {
    val xs = cube.keySet.map(_._1)
    val ys = cube.keySet.map(_._2)
    s"z = $z\n" +
    (ys.min to ys.max).map{y =>
      (xs.min to xs.max).map(x => cube.getOrElse((x, y, z), '.')).mkString
    }.mkString("\n")
  }
}
