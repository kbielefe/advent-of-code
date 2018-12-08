package common
import cats.data.State

object StateUtils {
  def repeatN[S, A](n: Int)(s: State[S, A]): State[S, List[A]] = {
    if (n == 0)
      State((s: S) => (s, List.empty[A]))
    else
      s flatMap {a => repeatN(n - 1)(s) map {a :: _}}
  }
}
