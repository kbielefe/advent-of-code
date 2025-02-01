package advent2022

object Day7:
  def part1(input: List[String]): Int =
    val sizes = parse(Nil, input).toSeq.groupMapReduce(_._1)(_._2)(_ + _)
    sizes.map(_._2).filter(_ <= 100000).sum

  def part2(input: List[String]): Int =
    val sizes = parse(Nil, input).toSeq.groupMapReduce(_._1)(_._2)(_ + _)
    val usedSpace = sizes(Nil)
    val needToFree = 30000000 - (70000000 - usedSpace)
    sizes.map(_._2).filter(_ >= needToFree).min

  type Dir = List[String]
  type Size = Int

  private val cdDir = """\$ cd (\S+)""".r
  private val cdUp  = """\$ cd \.\.""".r
  private val file  = """(\d+) (\S+)""".r

  private def parse(pwd: Dir, input: List[String]): Iterator[(Dir, Size)] =
    if input.isEmpty then
      Iterator.empty
    else
      input.head match
        case cdUp()           => parse(pwd.tail, input.tail)
        case cdDir(dir)       => parse(dir :: pwd, input.tail)
        case file(size, name) => pwd.tails.map((_, size.toInt)) ++ parse(pwd, input.tail)
        case _                => parse(pwd, input.tail)
