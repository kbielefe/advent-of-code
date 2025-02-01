package advent2022

object Day9:
  def part1(input: List[String]): Int =
    tailPositions(headPositions(expanded(input))).toSet.size

  def part2(input: List[String]): Int =
    Iterator.iterate(headPositions(expanded(input)))(tailPositions).drop(9).next.toSet.size

  def expanded(input: List[String]): Iterator[String] =
    input.iterator.flatMap{move =>
      val Array(direction, count) = move.split(" ")
      Iterator.fill(count.toInt)(direction)
    }

  def headPositions(expanded: Iterator[String]): Iterator[(Int, Int)] =
    expanded.scanLeft((0, 0)){
      case ((x, y), "U") => (x, y + 1)
      case ((x, y), "D") => (x, y - 1)
      case ((x, y), "L") => (x - 1, y)
      case ((x, y), "R") => (x + 1, y)
    }

  def tailPositions(headPositions: Iterator[(Int, Int)]): Iterator[(Int, Int)] =
    headPositions.scanLeft((0, 0)){case ((x, y), (headX, headY)) =>
      val touching = Math.abs(headX - x) <= 1 && Math.abs(headY - y) <= 1
      val xOffset = if touching then 0 else Math.signum(headX.toFloat - x.toFloat)
      val yOffset = if touching then 0 else Math.signum(headY.toFloat - y.toFloat)
      (x + xOffset.toInt, y + yOffset.toInt)
    }
