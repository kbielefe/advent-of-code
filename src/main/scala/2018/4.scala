package advent2018
import common.Day
import scala.io.Source

class Day4(source: Source) extends Day {
  val input = source.getLines().toList.sorted

  case class Interval(timestamp: String, guard: Int, first: Int, last: Int)

  val guardLine  = """\[\d\d\d\d-\d\d-\d\d \d\d:\d\d\] Guard #(\d+) begins shift""".r
  val asleepLine = """\[\d\d\d\d-\d\d-\d\d \d\d:(\d\d)\] falls asleep""".r
  val wakesLine  = """\[\d\d\d\d-\d\d-\d\d \d\d:(\d\d)\] wakes up""".r
  val timestamp  = """\[(\d\d\d\d-\d\d-\d\d \d\d:\d\d)\].*""".r

  def getTimestamp(line: String): String = line match {
    case timestamp(t) => t
  }

  val (_, _, intervals) = input.foldLeft((0, 0, Set.empty[Interval])){case ((guard, start, intervals), line) =>
    line match {
      case guardLine(newGuard) => (newGuard.toInt, 0, intervals)
      case asleepLine(minute)  => (guard, minute.toInt, intervals)
      case wakesLine(minute)   => (guard, 0, intervals + Interval(getTimestamp(line), guard, start, minute.toInt))
    }
  }

  def mostFrequentMinute(intervals: List[Interval]): (Int, Int) = {
    val allMinutes = intervals flatMap {interval => (interval.first until interval.last).toList}
    val frequencies = allMinutes.groupBy(identity).view.mapValues(_.length)
    frequencies.maxBy{_._2}
  }

  val grouped = intervals.toList groupBy {_.guard}
  val minutesAsleep = grouped.view.mapValues{_.map{interval => interval.last - interval.first}.sum}
  val sleepiestMinutes = minutesAsleep.maxBy{_._2}._2
  val sleepiestGuards = minutesAsleep.filter{_._2 == sleepiestMinutes}
  val sleepiestGuard = sleepiestGuards.minBy{_._1}._1
  val allMinutes = grouped(sleepiestGuard).toList flatMap {interval => (interval.first until interval.last).toList}
  val frequency = allMinutes.groupBy(identity).view.mapValues(_.length)
  val largestFrequency = frequency.maxBy{_._2}._2
  val minutesWithLargestFrequency = frequency.filter{_._2 == largestFrequency}.map{_._1}
  override def answer1 = (sleepiestGuard * minutesWithLargestFrequency.min).toString

  val (guard2, (minute, _)) = grouped.view.mapValues(mostFrequentMinute).maxBy{_._2._2}
  override def answer2 = (guard2 * minute).toString
}
