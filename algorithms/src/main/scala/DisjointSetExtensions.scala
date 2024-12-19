package algorithms

import cats.collections.DisjointSets

/** Extensions for DisjointSets (union-find) */
extension [T](dsets: DisjointSets[T])
  def inSameSet(a: T, b: T): (DisjointSets[T], Boolean) =
    val (asets, optALabel) = dsets.find(a)
    val (bsets, optBLabel) = asets.find(b)
    val same = for
      aLabel <- optALabel
      bLabel <- optBLabel
    yield aLabel == bLabel
    (bsets, same.getOrElse(false))

  /** Add a collection of elements that are all connected together */
  def addConnected(xs: IterableOnce[T]): DisjointSets[T] =
    val it = xs.iterator
    if it.isEmpty then
      dsets
    else
      val init = it.next
      it.foldLeft(dsets + init): (dsets, element) =>
        (dsets + element).union(init, element)._1

  /** Add t to the set, and connect it to any of the neighbors that already exist in the set */
  def addAndConnect(t: T, neighbors: IterableOnce[T]): DisjointSets[T] =
    neighbors.iterator.foldLeft(dsets + t)((sets, neighbor) => sets.union(t, neighbor)._1)
