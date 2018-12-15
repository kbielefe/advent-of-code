package common
import cats.data.{State, IndexedStateT}
import cats.Eval

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

  def foreach[S,A,B](s: State[(S, B), A]): State[(S, List[B]), List[A]] = {
    State.get flatMap {case (_, l) =>
      if (l.isEmpty) {
        State.pure(List.empty[A]).modify(x => (x._1, List.empty[B]))
      } else {
        for {
          _ <- IndexedStateT.modify[Eval, (S, List[B]), (S, B)](x => (x._1, l.head))
          a <- s
          _ <- IndexedStateT.modify[Eval, (S, B), (S, List[B])](x => (x._1, l.tail))
          r <- foreach(s)
        } yield a :: r
      }
    }
  }
}
