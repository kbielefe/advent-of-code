package day19
import parse.{*, given}
import algorithms.{Tree, Intervals}
import scala.annotation.tailrec
import Tree.*

case class XmasIntervals(x: Intervals[Int], m: Intervals[Int], a: Intervals[Int], s: Intervals[Int]):
  def combinations: Long =
    x.size.toLong * m.size.toLong * a.size.toLong * s.size.toLong

case class Condition(attribute: Char, op: Char, amount: Int):
  def succeeds(part: Part): Boolean = (attribute, op) match
    case ('x', '<') if part.x < amount => true
    case ('m', '<') if part.m < amount => true
    case ('a', '<') if part.a < amount => true
    case ('s', '<') if part.s < amount => true
    case ('x', '>') if part.x > amount => true
    case ('m', '>') if part.m > amount => true
    case ('a', '>') if part.a > amount => true
    case ('s', '>') if part.s > amount => true
    case _ => false

  def term(intervals: XmasIntervals): XmasIntervals = (attribute, op) match
    case ('x', '<') => intervals.copy(x = intervals.x.trimAbove(amount-1))
    case ('m', '<') => intervals.copy(m = intervals.m.trimAbove(amount-1))
    case ('a', '<') => intervals.copy(a = intervals.a.trimAbove(amount-1))
    case ('s', '<') => intervals.copy(s = intervals.s.trimAbove(amount-1))
    case ('x', '>') => intervals.copy(x = intervals.x.trimBelow(amount+1))
    case ('m', '>') => intervals.copy(m = intervals.m.trimBelow(amount+1))
    case ('a', '>') => intervals.copy(a = intervals.a.trimBelow(amount+1))
    case ('s', '>') => intervals.copy(s = intervals.s.trimBelow(amount+1))

  def invertedTerm(intervals: XmasIntervals): XmasIntervals = (attribute, op) match
    case ('x', '>') => intervals.copy(x = intervals.x.trimAbove(amount))
    case ('m', '>') => intervals.copy(m = intervals.m.trimAbove(amount))
    case ('a', '>') => intervals.copy(a = intervals.a.trimAbove(amount))
    case ('s', '>') => intervals.copy(s = intervals.s.trimAbove(amount))
    case ('x', '<') => intervals.copy(x = intervals.x.trimBelow(amount))
    case ('m', '<') => intervals.copy(m = intervals.m.trimBelow(amount))
    case ('a', '<') => intervals.copy(a = intervals.a.trimBelow(amount))
    case ('s', '<') => intervals.copy(s = intervals.s.trimBelow(amount))

case class Rule(condition: Option[Condition], action: String):
  def succeeds(part: Part): Boolean =
    condition.map(_.succeeds(part)).getOrElse(true)

  def term(intervals: XmasIntervals): XmasIntervals =
    condition.map(_.term(intervals)).getOrElse(intervals)

  def invertedTerm(intervals: XmasIntervals): XmasIntervals =
    condition.map(_.invertedTerm(intervals)).getOrElse(intervals)

case class Workflow(name: String, rules: List[Rule]):
  def result(part: Part): String =
    rules.find(_.succeeds(part)).map(_.action).get

  def terms(intervals: XmasIntervals, next: String): List[XmasIntervals] =
    def helper(rules: List[Rule], intervals: XmasIntervals, accum: List[XmasIntervals]): List[XmasIntervals] =
      if rules.isEmpty then
        accum
      else
        if rules.head.action == next then
          helper(rules.tail, rules.head.invertedTerm(intervals), rules.head.term(intervals) :: accum)
        else
          helper(rules.tail, rules.head.invertedTerm(intervals), accum)
    helper(rules, intervals, List.empty)

case class Part(x: Int, m: Int, a: Int, s: Int):
  def allRatings: Int = x + m + a + s
  def accepted(workflowsByName: Map[String, Workflow]): Boolean =
    Iterator.iterate("in"){workflowName =>
      workflowsByName(workflowName).result(this)
    }.dropWhile(name => name != "R" && name != "A").next == "A"

case class Input(workflows: List[Workflow], parts: List[Part])

given Read[Rule] with
  def read(input: String): Rule =
    input.split(":") match
      case Array(condition, action) => Rule(Some(summon[Read[Condition]].read(condition)), action)
      case Array(action)            => Rule(None, action)

given Read[Condition] = Read("""(.)(.)(\d+)""".r)
given Read[Workflow] = Read("""([^{]+)\{(.+)\}""".r)
given Read[Part] = Read("""\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}""".r)
given Read[Input] = Read("""(?s)(.+)\n\n(.+)""".r)
given lr: Read[List[Rule]] = Read(",")
given lw: Read[List[Workflow]] = Read("\n")
given lp: Read[List[Part]] = Read("\n")

object Puzzle extends runner.Day[Input, Long, Long]:
  def part1(input: Input): Long =
    val workflowsByName = input.workflows.map(workflow => workflow.name -> workflow).toMap
    input.parts.filter(_.accepted(workflowsByName)).map(_.allRatings).sum

  def part2(input: Input): Long =
    val a = Workflow("A", List.empty)
    val r = Workflow("R", List.empty)
    given Tree[Workflow] = Tree.fromId(a :: r :: input.workflows, _.name, _.rules.map(_.action).distinct)
    val start = input.workflows.find(_.name == "in").get
    val intervals = XmasIntervals(Intervals(1, 4000), Intervals(1, 4000), Intervals(1, 4000), Intervals(1, 4000))
    def terms = start.allPathsTo(_.name == "A").map(_.reverse).flatMap(createTerms(intervals))
    terms.map(_.combinations).sum

  def createTerms(intervals: XmasIntervals)(path: List[Workflow]): List[XmasIntervals] =
    if path.size <= 1 then
      List(intervals)
    else
      path.head.terms(intervals, path.drop(1).head.name).flatMap(intervals =>
        createTerms(intervals)(path.tail)
      )
