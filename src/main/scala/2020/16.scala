package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day16 extends MultilineStringsDay[Int, Long](2020, 16) {
  override def part1(input: Observable[Seq[String]]): Task[Int] =
    input.toListL.map{parts =>
      val fields = parseFields(parts(0))
      val nearby = parseTicket(parts(2).drop(1))
      errorRate(fields, nearby)
    }

  override def part2(input: Observable[Seq[String]]): Task[Long] =
    input.toListL.map{parts =>
      val fields = parseFields(parts(0))
      val myTicket = parseTicket(parts(1).drop(1))
      val nearby = parseTicket(parts(2).drop(1))
      val validNearby = filterInvalid(fields, nearby)
      val validated = validateFields(fields, myTicket ++ validNearby)
      val correlated = correlateFields(validated)
      correlated.map(_.head.name).zip(myTicket.head).filter(_._1.startsWith("departure")).map(_._2.toLong).product
    }


  private case class Field(name: String, min1: Int, max1: Int, min2: Int, max2: Int) {
    def valid(field: Int): Boolean = (field >= min1 && field <= max1) || (field >= min2 && field <= max2)
  }

  private def parseFields(input: Seq[String]): Seq[Field] =
    input.map{
      case s"$name: $min1-$max1 or $min2-$max2" => Field(name, min1.toInt, max1.toInt, min2.toInt, max2.toInt)
    }

  private def parseTicket(input: Seq[String]): Seq[Array[Int]] =
    input.map(_.split(",").map(_.toInt))

  private def errorRate(fields: Seq[Field], nearby: Seq[Array[Int]]): Int =
    nearby.map{ticket =>
      ticket.map{field =>
        if (fields.exists(_.valid(field)))
          0
        else
          field
      }.sum
    }.sum

  private def filterInvalid(fields: Seq[Field], nearby: Seq[Array[Int]]): Seq[Array[Int]] =
    nearby.filter{ticket =>
      ticket.forall{field =>
        fields.exists(_.valid(field))
      }
    }

  private def validateFields(fields: Seq[Field], tickets: Seq[Array[Int]]): Seq[Set[Field]] = {
    val init = Seq.fill(tickets.head.size)(fields.toSet)
    tickets.foldLeft(init){case (possibleFields, ticket) =>
      possibleFields.zip(ticket).map{case (fields, ticketField) =>
        fields.filter(_.valid(ticketField))
      }
    }
  }

  @scala.annotation.tailrec
  private def correlateFields(validated: Seq[Set[Field]]): Seq[Set[Field]] = {
    val singles = validated.filter(_.size == 1).toSet.flatten
    if (singles.size == validated.size)
      validated
    else
      correlateFields(validated.map(fields => if (fields.size == 1) fields else fields -- singles))
  }
}
