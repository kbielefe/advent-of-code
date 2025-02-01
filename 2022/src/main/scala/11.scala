package advent2022
import puzzleparse.MultiLine
import scala.collection.mutable.Queue

object Day11:
  def part1(input: MultiLine[List[String]]): Long = answer(input, 20, 3)

  def part2(input: MultiLine[List[String]]): Long = answer(input, 10000, 1)

  private def answer(input: MultiLine[List[String]], rounds: Int, divisor: Int): Long =
    val monkeyMap = input.map(parseMonkey).toMap
    val mod = monkeyMap.map(_._2.test).product
    monkeyBusiness(Iterator.iterate(monkeyMap)(round(mod, divisor)).drop(rounds).next)

  private type MonkeyMap = Map[Int, Monkey]

  private def monkeyBusiness(map: MonkeyMap): Long =
    map.map(_._2.inspectCount).toList.sorted.reverse.take(2).map(_.toLong).product

  private def round(mod: Long, divisor: Long)(map: MonkeyMap): MonkeyMap =
    map.keys.toSeq.sorted.foldLeft(map)((map, key) => map(key).turn(map, mod, divisor))

  private case class Monkey(number: Int, items: Queue[Long], operation: Long => Long, test: Long, throwTrue: Int, throwFalse: Int, inspectCount: Long):
    def turn(map: MonkeyMap, mod: Long, divisor: Long): MonkeyMap =
      items.foldLeft(map){(map, item) =>
        val newItem = (operation(item) / divisor) % mod
        val throwTo = if newItem % test == 0 then throwTrue else throwFalse
        map(throwTo).catchItem(map, newItem)
      } + (number -> copy(items = Queue.empty, inspectCount = inspectCount + items.size))

    def catchItem(map: MonkeyMap, item: Long): MonkeyMap =
      map + (number -> copy(items = items.appended(item)))

  private def parseMonkey(input: List[String]): (Int, Monkey) =
    val s"Monkey $number:"                       = input(0): @unchecked
    val s"Starting items: $items"                = input(1): @unchecked
    val s"Operation: new = $operation"           = input(2): @unchecked
    val s"Test: divisible by $test"              = input(3): @unchecked
    val s"If true: throw to monkey $throwTrue"   = input(4): @unchecked
    val s"If false: throw to monkey $throwFalse" = input(5): @unchecked
    number.toInt -> Monkey(number.toInt, Queue.from(items.split(", ").map(_.toLong)), parseOperation(operation), test.toLong, throwTrue.toInt, throwFalse.toInt, 0)

  private def parseOperation(operation: String)(input: Long): Long =
    operation match
      case "old * old"     => input * input
      case s"old * $value" => input * value.toLong
      case s"old + $value" => input + value.toLong
