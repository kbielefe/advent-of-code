package algorithms

import scala.collection.mutable

type Memoized[K, V] = Cache[K, V] ?=> V

opaque type Cache[K, V] = mutable.Map[K, V]

extension [K, V] (c: Cache[K, V])
  def get(key: K): Option[V] = c.get(key)
  def +=(elem: (K, V)): Unit = c += elem

object Cache:
  def empty[K, V]: Cache[K, V] =
    mutable.Map.empty

object Memoize:
  def apply[K, V](key: K, value: => V)(using c: Cache[K, V]): V =
    c.get(key) match
      case Some(cached) => cached
      case None =>
        val result = value
        c += (key, result)
        result
