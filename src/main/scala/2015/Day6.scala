package advent2015

object Day6:
  def part1(input: List[String]): Int =
    input.foldLeft(Set.empty[(Int, Int)]){
      case (lights, s"turn off $a,$b through $x,$y") => lights -- set(a, b, x, y)
      case (lights, s"turn on $a,$b through $x,$y") => lights ++ set(a, b, x, y)
      case (lights, s"toggle $a,$b through $x,$y") =>
        val toToggle = set(a, b, x, y)
        val turnOff = lights & toToggle
        val turnOn = toToggle -- lights
        lights -- turnOff ++ turnOn
    }.size

  def part2(input: List[String]): Long =
    input.foldLeft[Map[(Int, Int), Long]](Map.empty[(Int, Int), Long].withDefault(_ => 0)){
      case (lights, s"turn off $a,$b through $x,$y") => set(a, b, x, y).foldLeft(lights){case (lights, pos) => lights.updated(pos, Math.max(lights(pos) - 1, 0))}
      case (lights, s"turn on $a,$b through $x,$y") => set(a, b, x, y).foldLeft(lights){case (lights, pos) => lights.updated(pos, lights(pos) + 1)}
      case (lights, s"toggle $a,$b through $x,$y") => set(a, b, x, y).foldLeft(lights){case (lights, pos) => lights.updated(pos, lights(pos) + 2)}
    }.map(_._2).sum

  private def set(a: String, b: String, x: String, y: String): Set[(Int, Int)] =
    val result = for
      x <- a.toInt to x.toInt
      y <- b.toInt to y.toInt
    yield (x, y)
    result.toSet
