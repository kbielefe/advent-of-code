package common

import cats.effect._
import cats.implicits._
import colibri.ext.monix._
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

object Runner extends TaskApp {
  private val days = List(
    advent2019.Day1,
    advent2020.Day1,
    advent2020.Day2
  )

  private val years = days.map(_.year.toString).toSet.toList.sorted
  private val daysForYear = days.groupBy(_.year).view.map{case (year, days) => (year.toString, days.map(_.day.toString))}.toMap
  private val daysFromYearAndDay = days.map(day => ((day.year.toString, day.day.toString), day)).toMap

  def run(args: List[String]): Task[ExitCode] = for {
    outwatch <- outwatchSync.to[Task]
    _        <- OutWatch.renderReplace[Task]("#outwatch", outwatch)
  } yield ExitCode.Success

  private val outwatchSync = for {
    year   <- Handler.create[String](years.last.toString)
    day    <- Handler.create[String](daysForYear(years.last).last.toString)
    part   <- Handler.create[String]("1")
    answer <- Handler.create[String]("")
  } yield div(
    label(`for` := "year", " Year: "),
    select(
      idAttr := "year",
      selectLast(years),
      onChange.value --> year
    ),
    label(`for` := "day", " Day: "),
    select(
      idAttr := "day",
      year.map(y => selectLast(daysForYear(y))),
      onChange.value --> day,
      emitter(year.map(daysForYear(_).last)) --> day
    ),
    label(`for` := "part", " Part: "),
    select(
      idAttr := "part",
      option("1", selectOnChange(Observable.combineLatest2(year, day))),
      option("2"),
      onChange.value --> part,
      emitter(year.map(_ => "1")) --> part,
      emitter(day.map(_ => "1")) --> part
    ),
    " ",
    button("Run", runPuzzle(year, day, part, answer)),
    " ",
    input(`type` := "text", readOnly, value <-- answer),
    " ",
    button("Copy", copyAnswer(answer)),
    // time
    br(),
    textArea(idAttr := "input", getInput(year, day))
    // visualization
  )

  private def runPuzzle(year: Handler[String], day: Handler[String], part: Handler[String], answer: Handler[String]): VDomModifier =
    onClick
      .withLatest(year)
      .withLatest(day)
      .withLatest(part)
      .foreach{case (((_, year), day), part) => println(s"Running $year/$day/$part")}

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

  private def exceptionString(e: Throwable): String = {
    val os = new java.io.ByteArrayOutputStream()
    val ps = new java.io.PrintStream(os)
    e.printStackTrace(ps)
    ps.flush()
    val result = os.toString
    ps.close()
    os.close()
    result
  }

  private def runClicked(e: dom.Event): Unit = {
    val answer = document.getElementById("answer").asInstanceOf[dom.html.Input]
    val input = document.getElementById("input").asInstanceOf[dom.html.TextArea]
    val partSelect = document.getElementById("part").asInstanceOf[dom.html.Select]
    val time = document.getElementById("time")
    val visualization = document.getElementById("visualization").asInstanceOf[dom.html.Div]
    val yearSelect = document.getElementById("year").asInstanceOf[dom.html.Select]
    val daySelect = document.getElementById("day").asInstanceOf[dom.html.Select]

    visualization.innerHTML = ""
    val dayOpt = days.find(day => day.year == yearSelect.value.toInt && day.day == daySelect.value.toInt)
    dayOpt.map{day =>
      val processedInput = day.input(input.value)
      val part = if (partSelect.value == "1") day.part1 _ else day.part2 _
      part(processedInput).timed.foreachL{result =>
        answer.value = result._2.toString
        time.textContent = s"${result._1.toMillis} ms"
        }.onErrorHandle{e =>
          visualization.innerHTML = s"<pre>${exceptionString(e)}</pre>"
        }
          .runToFuture
    }
  }
}
