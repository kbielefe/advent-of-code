package common
import cats.data.State

object StateUtils {
  def repeatN[S, A](n: Int)(f: S => (S, A)): State[S, List[A]] = {
    def iteration(input: (S, List[A])): (S, List[A]) = {
      val (state, value) = f(input._1)
      (state, value :: input._2)
    }
    def iterations(s: S): Iterator[(S, List[A])] = Iterator.iterate((s, List.empty[A]))(iteration)
    def lastIteration(s: S): (S, List[A]) = iterations(s).drop(n).next
    State(lastIteration)
  }
}
