package algorithms

import scala.math.Integral.Implicits.infixIntegralOps

extension [K, V](i: Map[K, Set[V]])
  def addMulti(k: K, v: V): Map[K, Set[V]] =
    i + ((k, i.getOrElse(k, Set.empty) + v))

  def delMulti(k: K, v: V): Map[K, Set[V]] =
    val newSet = i.get(k).fold(Set.empty)(_ - v)
    if newSet.isEmpty then
      i - k
    else
      i + (k -> newSet)

extension[K, N](map: Map[K, N])(using n: Integral[N])
  def increment(key: K, amount: N): Map[K, N] =
    map + (key -> (map.getOrElse(key, n.zero) + amount))

  def decrement(key: K, amount: N): Map[K, N] =
    map + (key -> (map.getOrElse(key, n.zero) - amount))
