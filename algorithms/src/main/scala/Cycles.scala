package algorithms
import math.Numeric.Implicits.given
import scala.annotation.tailrec

/*
 * Returns (number of elements before cycle starts, period of cycle)
 *
 * numReps indicates the number of times the cycle must repeat in order to be
 * considered a cycle.
 */
def detectCycle[A](it: Iterator[A], numReps: Int = 4): Option[(Int, Int)] =
  def hasReps(list: LazyList[A], index: Int)(start: (Int, LazyList[A])): Boolean =
    val (startIndex, startList) = start
    val size = (index - startIndex) * numReps
    list.take(size).sameElements(startList.take(size))

  @tailrec
  def naiveDetectCycle(list: LazyList[A], seen: Map[A, Vector[(Int, LazyList[A])]], index: Int): Option[(Int, Int)] =
    if list.isEmpty then
      None
    else
      val element = list.head
      val result = for
        starts <- seen.get(element)
        start  <- starts.find(hasReps(list, index))
        startIndex = start._1
      yield (startIndex, index - startIndex)
      result match
        case Some(answer) => Some(answer)
        case None => naiveDetectCycle(list.tail,
          seen + (element -> seen.getOrElse(element, Vector.empty).appended((index, list))),
          index + 1
        )

  naiveDetectCycle(LazyList.from(it), Map.empty, 0)
end detectCycle

/* Given the number of iterations before a cycle starts, the length of the cycle,
 * as returned by detectCycle, and the huge target you want to hit, return the
 * smaller equivalent number of iterations.
 */
def cycledEquivalentIterations(start: Long, cycle: Long, target: Long): Long =
  (target - start) % cycle + start

/* Given the number of iterations before a cycle starts, the length of the cycle,
 * as returned by detectCycle, the huge target you want to hit, and a function
 * that returns the value after the given number of iterations, return the
 * smaller value after the huge target of iterations.
 *
 * Assumes a constant increase in N every period.
 */
def cycledEquivalentValue(start: Long, cycle: Long, target: Long, f: Int => Int): Long =
  def longF(in: Long): Long = f(in.toInt).toLong
  val valueAtStart = longF(start)
  val valueAtEquivalent = longF(cycledEquivalentIterations(start, cycle, target))
  val increaseAtEquivalent = valueAtEquivalent - valueAtStart
  val valueAtEnd = longF(start + cycle)
  val numberOfPeriods = (target - start) / cycle
  val increasePerPeriod = valueAtEnd - valueAtStart
  valueAtStart + numberOfPeriods * increasePerPeriod + increaseAtEquivalent
