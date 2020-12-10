package common

import cats.effect._
import cats.implicits._
import colibri.ext.monix._
import monix.catnap.ConcurrentQueue
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._
import outwatch._
import outwatch.dsl._
import outwatch.reactive.handlers.monix._
import scala.concurrent.duration._
import scala.util.{Try, Success}

object Runner extends TaskApp {
  private val days = List(
    advent2018.Day23,
    advent2019.Day1,
    advent2020.Day1,
    advent2020.Day2,
    advent2020.Day3,
    advent2020.Day4,
    advent2020.Day5,
    advent2020.Day6,
    advent2020.Day7,
    advent2020.Day8,
    advent2020.Day9
  )

  private val years = days.map(_.year.toString).toSet.toList.sorted
  private val daysForYear = days.groupBy(_.year).view.map{case (year, days) => (year.toString, days.map(_.day.toString))}.toMap
  private val daysFromYearAndDay = days.map(day => ((day.year.toString, day.day.toString), day)).toMap

  def run(args: List[String]): Task[ExitCode] = for {
    outwatch <- outwatchSync.to[Task]
    _        <- OutWatch.renderReplace[Task]("#outwatch", outwatch)
  } yield ExitCode.Success

  private def outwatchSync = for {
    year          <- Handler.create[String](years.last.toString)
    day           <- Handler.create[String](daysForYear(years.last).last.toString)
    part          <- Handler.create[String]("1")
    answer        <- Handler.create[String]("")
    time          <- Handler.create[String]("")
    timedAnswer   <- Handler.create[(String, Try[String])](("", Success("")))
    puzzleInput   <- Handler.create[String]("")
    visualization <- Handler.create[Visualization.Event](Visualization.Clear)
  } yield div(
    div(
    label(`for` := "year", " Year: "),
    select(
      idAttr := "year",
      selectLast(years),
      onChange.value --> year,
      onChange.use("") --> answer,
      onChange.use("") --> time,
      onChange.use(Visualization.Clear) --> visualization
    ),
    label(`for` := "day", " Day: "),
    select(
      idAttr := "day",
      year.map(y => selectLast(daysForYear(y))),
      onChange.value --> day,
      onChange.use("") --> answer,
      onChange.use("") --> time,
      onChange.use(Visualization.Clear) --> visualization,
      emitter(year.map(daysForYear(_).last)) --> day
    ),
    label(`for` := "part", " Part: "),
    select(
      idAttr := "part",
      option("1", selectOnChange(Observable.combineLatest2(year, day))),
      option("2"),
      onChange.value --> part,
      onChange.use("") --> answer,
      onChange.use("") --> time,
      onChange.use(Visualization.Clear) --> visualization,
      emitter(year.map(_ => "1")) --> part,
      emitter(day.map(_ => "1")) --> part
    ),
    " ",
    button(
      "[Run]",
      onClick.use("") --> answer,
      onClick.use("") --> time,
      onClick.use(Visualization.Clear) --> visualization,
      runPuzzle(year, day, part, puzzleInput, timedAnswer),
      emitter(timedAnswer.map(_._1)) --> time,
      emitter(timedAnswer.map(_._2).filter(_.isSuccess).map(_.get)) --> answer,
      emitter(timedAnswer.map(_._2).filter(_.isFailure).map(_.failed.get).map(Visualization.Error)) --> visualization
    ),
    " ",
    input(`type` := "text", readOnly, value <-- answer),
    " ",
    button("[Copy]", copyAnswer(answer)),
    " ",
    outwatch.dsl.span(cls := "time", time),
    div(
      cls := "links",
      a(href := "https://www.reddit.com/r/adventofcode/", "[Reddit]", target := "_blank"),
      a(href <-- year.map(y => s"https://adventofcode.com/$y/leaderboard/private/view/147910"), "[Leaderboard]", target := "_blank"),
      a(href <-- Observable.combineLatest2(year, day).map{case (y, d) => s"https://adventofcode.com/$y/day/$d"}, "[Puzzle]", target := "_blank"),
      a(href <-- year.map(y => s"https://adventofcode.com/$y"), "[Advent Calendar]", target := "_blank"),
      a(href := "https://github.com/kbielefe/advent-of-code", "[GitHub]", target := "_blank"),
    ),
    ),
    div(
      cls := "input",
      label(`for` := "input", idAttr := "input-label", "Input:"),
      textArea(
        idAttr := "input",
        spellCheck := false,
        value <-- puzzleInput,
        emitter(getInput(year, day)) --> puzzleInput,
        onChange.value --> puzzleInput,
        onKeyUp.value --> puzzleInput
      ),
      br,
      outwatch.dsl.span(
        cls := "lines",
        puzzleInput
          .map(_.linesIterator.length)
          .map(length => if (length == 0) "" else if (length == 1) "1 line" else s"$length lines")
      )
    ),
    div(
      idAttr := "visualization",
      Visualization(visualization)
    )
  )

  private def runPuzzle(year: Handler[String], day: Handler[String], part: Handler[String], puzzleInput: Handler[String], timedAnswer: Handler[(String, Try[String])]): VDomModifier =
    onClick
      .withLatest(year)
      .withLatest(day)
      .withLatest(part)
      .withLatest(puzzleInput)
      .concatMapAsync{case ((((_, year), day), part), input) => runPart(year, day, part, input)} --> timedAnswer

  private def runPart(year: String, day: String, part: String, input: String): Task[(String, Try[String])] = {
    val task = for {
      puzzle         <- Task(daysFromYearAndDay((year, day)))
      processedInput <- Task(puzzle.input(input))
      partFunction   <- if (part == "1") Task(puzzle.part1Task _) else Task(puzzle.part2Task _)
      result         <- partFunction(processedInput)
    } yield result
    task.materialize.timed.map{case (duration, answer) => (s"${duration.toMillis} milliseconds", answer.map(_.toString))}
  }

  private def copyAnswer(answer: Handler[String]): VDomModifier =
    onClick.withLatest(answer).foreach{case (_, toCopy) => dom.window.navigator.clipboard.writeText(toCopy)}

  private def getInput(year: Observable[String], day: Observable[String]): Observable[String] =
    Observable
      .combineLatest2(year, day)
      .debounce(100.millis)
      .mapEval{case (year, day) => Task.async{callback =>
        val xhr = new dom.XMLHttpRequest()
        xhr.open("GET", s"input/$year/$day.txt")
        xhr.onload = { (e: dom.Event) =>
          if (xhr.status == 200) {
            callback.onSuccess(xhr.responseText)
          } else {
            callback.onSuccess(s"Error retrieving input: ${xhr.status} ${xhr.statusText}")
          }
        }
        xhr.send()
      }}

  private def selectOnChange[A](handler: Observable[A]): VDomModifier =
    selected <-- handler.map(_ => true)

  private def selectLast(options: List[String]): List[VNode] =
    options.init.map(option(_)) :+ option(options.last, selected)
}
