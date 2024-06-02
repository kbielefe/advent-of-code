package algorithms

extension [K, V](i: Map[K, Set[V]])
  def addMulti(k: K, v: V): Map[K, Set[V]] =
    i + ((k, i.getOrElse(k, Set.empty) + v))

  def delMulti(k: K, v: V): Map[K, Set[V]] =
    val newSet = i.get(k).fold(Set.empty)(_ - v)
    if newSet.isEmpty then
      i - k
    else
      i + (k -> newSet)
