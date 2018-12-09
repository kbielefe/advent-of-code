package common
import cats.data.State

object StateUtils {
  def repeatN[S, A](n: Int)(s: State[S, A]): State[S, List[A]] = {
    if (n == 0)
      State((s: S) => (s, List.empty[A]))
    else
      s flatMap {a => repeatN(n - 1)(s) map {a :: _}}
  }

  def repeatWhile[S, A](p: S => Boolean)(s: State[S, A]): State[S, List[A]] = {
    State((s: S) => (s, p(s))) flatMap {a =>
      if (a)
        s flatMap {a => repeatWhile(p)(s) map {a :: _}}
      else
        State((s: S) => (s, List.empty[A]))
    }
  }
}
