package algorithms

import cats.Applicative
import cats.syntax.functor.toFunctorOps
import scala.collection.mutable

type Memoized[K, V]  = Cache[K, V] ?=> V
type MemoizedT[F[_], K, V] = Cache[K, V] ?=> F[V]

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

object MemoizeT:
  def apply[F[_], K, V](key: K, value: => F[V])(using c: Cache[K, V], F: Applicative[F]): F[V] =
    c.get(key) match
      case Some(cached) => F.pure(cached)
      case None =>
        value.map{result =>
          c += (key, result)
          result
        }
