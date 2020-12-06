package common

import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import outwatch.VDomModifier

abstract class Day[I, A, B](val year: Int, val day: Int) {
  def input(string: String): I
  def part1(input: I): Task[A]
  def part2(input: I): Task[B]
  def vis1(input: I): Observable[VDomModifier] = Observable.empty
  def vis2(input: I): Observable[VDomModifier] = Observable.empty
}

abstract class IntsDay[A, B](year: Int, day: Int) extends Day[Observable[Int], A, B](year, day) {
  override def input(string: String): Observable[Int] =
    Observable.fromIterator(Task(string.linesIterator)).filter(!_.isEmpty).map(_.toInt)
}

abstract class StringsDay[A, B](year: Int, day: Int) extends Day[Observable[String], A, B](year, day) {
  override def input(string: String): Observable[String] =
    Observable.fromIterator(Task(string.linesIterator)).filter(!_.isEmpty)
}

abstract class GridDay[A, B](year: Int, day: Int) extends Day[Map[(Int, Int), Char], A, B](year, day) {
  override def input(string: String): Map[(Int, Int), Char] =
    string.linesIterator.filter(!_.isEmpty).zipWithIndex.flatMap{case (line, y) =>
      line.iterator.zipWithIndex.map{case (char, x) => ((x, y) -> char)}
    }.toMap
}

object Multiline {
  def apply(string: String): Observable[Seq[String]] =
    Observable
      .fromIterator(Task(string.linesIterator))
      .append("")
      .scan((Seq.empty[String], false)){case ((seq, complete), next) =>
        if (next.isEmpty) (seq, true) else if (complete) (Seq(next), false) else (seq.appended(next), false)
      }.filter(_._2)
      .map(_._1)
      .filter(!_.isEmpty)
}

abstract class MultilineStringsDay[A, B](year: Int, day: Int) extends Day[Observable[Seq[String]], A, B](year, day) {
  override def input(string: String): Observable[Seq[String]] =
    Multiline(string)
}

abstract class MapDay[A, B](year: Int, day: Int) extends Day[Observable[Map[String, String]], A, B](year, day) {
  override def input(string: String): Observable[Map[String, String]] =
    Multiline(string)
      .map(_.mkString(" "))
      .map{line =>
        val pairs = line.split(" ")
        val fields = pairs.map{pair =>
          val Array(x, y) = pair.split(":")
          (x, y)
        }
        fields.toMap
      }
}

