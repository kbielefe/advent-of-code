package common

import cats.effect._
import cats.implicits._
import monix.eval._
import monix.reactive.Observable
import org.scalajs.dom
import scala.io.Source

object Runner extends TaskApp {
  def run(args: List[String]): Task[ExitCode] =
    inputLines(2019, 1).foreachL(println).as(ExitCode.Success)

  private def inputLines(year: Int, day: Int): Observable[String] =
    Observable.fromIterator(input(year, day).map(_.linesIterator))

  private def input(year: Int, day: Int): Task[String] = Task.async{callback =>
    val xhr = new dom.XMLHttpRequest()
    xhr.open("GET", s"input/$year/$day.txt")
    xhr.onload = { (e: dom.Event) =>
      if (xhr.status == 200) {
        callback.onSuccess(xhr.responseText)
      } else {
        callback.onError(new Exception(s"Received status ${xhr.status} retrieving input for $year/$day"))
      }
    }
    xhr.send()
  }
}
