package algorithms
import math.Numeric.Implicits.given
import scala.annotation.tailrec

 /*
  * Returns (number of elements before cycle starts, period of cycle, repeated element)
  *
  * minPeriod might be helpful in situations like a cycle being detected before
  * all the input has been processed.
  */
 def detectCycle[A](it: Iterator[A], minPeriod: Int = 0): Option[(Int, Int, A)] = {
    @tailrec
    def detectSeen(it: Iterator[A], seen: Map[A, Int], count: Int): Option[(Int, Int, A)] = {
      if (!it.hasNext) {
        None
      } else {
        val curr = it.next
        seen.get(curr) match
          case Some(start) if count - start >= minPeriod => Some((start, count - start, curr))
          case Some(start)                               => detectSeen(it, seen, count + 1)
          case None                                      => detectSeen(it, seen + (curr -> count), count + 1)
      }
    }

    detectSeen(it, Map.empty[A, Int], 0)
  }

 /* Given the number of iterations before a cycle starts, the length of the cycle,
  * as returned by detectCycle, and the huge target you want to hit, return the
  * smaller equivalent number of iterations.
  */
 def cycledEquivalentIterations(start: Long, cycle: Long, target: Long): Long = {
   (target - start) % cycle + start
 }

 /* Given the number of iterations before a cycle starts, the length of the cycle,
  * as returned by detectCycle, the huge target you want to hit, and a function
  * that returns the value after the given number of iterations, return the
  * smaller value after the huge target of iterations.
  *
  * Assumes a constant increase in N every period.
  */
 def cycledEquivalentValue(start: Long, cycle: Long, target: Long, f: Long => Long): Long =
   val valueAtStart = f(start)
   val valueAtEquivalent = f(cycledEquivalentIterations(start, cycle, target))
   val increaseAtEquivalent = valueAtEquivalent - valueAtStart
   val valueAtEnd = f(start + cycle)
   val numberOfPeriods = (target - start) / cycle
   val increasePerPeriod = valueAtEnd - valueAtStart
   valueAtStart + numberOfPeriods * increasePerPeriod + increaseAtEquivalent
