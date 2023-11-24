package algorithms

import scala.annotation.tailrec

object QuineMcCluskey:
  opaque type Minterm = Int

  object Minterm:
    def apply(i: Int): Minterm =
      assert(i >= 0)
      i

    def fromString(string: String, p: Char => Boolean): Minterm =
      string.foldLeft(0)((accum, char) => if p(char) then (accum << 1) | 1 else accum << 1)

  extension (m: Minterm)
    def toBitSeq: Seq[Int] =
      Iterator.iterate(m)(_ >> 1).take(32).map(_ & 1).toSeq.reverse

  private case class Implicant(labels: String, minterms: Set[Minterm], bits: Seq[Option[Int]]):
    def this(labels: String, minterm: Minterm) =
      this(labels, Set(minterm), minterm.toBitSeq.map(Some(_)))

    def canMerge(other: Implicant): Boolean =
      bits.zip(other.bits).map((x, y) => if x != y then 1 else 0).sum == 1

    def merge(other: Implicant): Implicant =
      val newBits = bits.zip(other.bits).map{
        case (Some(x), Some(y)) if x == y => Some(x)
        case _ => None
      }
      new Implicant(labels, minterms ++ other.minterms, newBits)

    def oneCount: Int = bits.count(_ == Some(1))

    def bitString = bits.map{
      case Some(x) => x.toString
      case None    => "-"
    }.takeRight(labels.length).mkString

    def term: String = bits.takeRight(labels.length).zip(labels).flatMap{
      case (Some(1), label) => Some(s"$label")
      case (Some(0), label) => Some(s"${label}'")
      case _ => None
    }.mkString

    override def toString: String =
      s"Implicant((${minterms.toList.sorted.mkString(",")}), $bitString, $term)"

  def apply(labels: String, oneTerms: Set[Minterm], dontCares: Set[Minterm]): Unit =
    val implicants = (oneTerms ++ dontCares).map(new Implicant(labels, _))
    val primeImplicants = Iterator.iterate(implicants)(mergeImplicants).drop(labels.size).next
    val implicantsByMinterm = primeImplicants.flatMap(implicant => (implicant.minterms -- dontCares).map(minterm => (minterm, implicant))).groupMap(_._1)(_._2)
    val essentialPrimeImplicants = implicantsByMinterm.filter(_._2.size == 1).flatMap(_._2).toSet
    println("Essential implicants:")
    println(essentialPrimeImplicants.map(_.term).toList.sorted.mkString(" + "))
    val uncoveredMinterms = essentialPrimeImplicants.foldLeft(oneTerms)((oneTerms, accum) => oneTerms -- accum.minterms)
    println("Uncovered minterms:")
    uncoveredMinterms.foreach(println)

  private def mergeImplicants(implicants: Set[Implicant]): Set[Implicant] =
    val groups = implicants.groupBy(_.oneCount)
    val merged = (0 to groups.keys.max).sliding(2).flatMap{
      case Seq(x, y) if groups.contains(x) && groups.contains(y) => mergeGroups(groups(x), groups(y))
      case other => Set.empty
    }.toSet
    val involved = (0 to groups.keys.max).sliding(2).flatMap{
      case Seq(x, y) if groups.contains(x) && groups.contains(y) => involvedInMerge(groups(x), groups(y))
      case other => Set.empty
    }.toSet
    merged ++ implicants -- involved

  private def mergeGroups(xs: Set[Implicant], ys: Set[Implicant]): Set[Implicant] =
    for
      x <- xs
      y <- ys
      if x.canMerge(y)
    yield x.merge(y)

  private def involvedInMerge(xs: Set[Implicant], ys: Set[Implicant]): Set[Implicant] =
    val involved = for
      x <- xs
      y <- ys
      if x.canMerge(y)
    yield Set(x, y)
    involved.flatten
