package visualizations

import cats.Parallel
import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.std.Console
import cats.effect.unsafe.implicits.*
import io.circe.*
import io.circe.KeyDecoder
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.websocket.*
import org.http4s.dom.*
import org.http4s.syntax.all.*
import org.scalajs.dom.*, html.Element
import scala.concurrent.duration.*
import scala.scalajs.js.annotation.*

@JSExportTopLevel("AnimatedGrid")
object AnimatedGrid:
  @JSExport
  def animate(): Unit =
    (grid >> frames).unsafeRunAndForget()

  private val grid: IO[Unit] =
    val client = FetchClientBuilder[IO].create
    client.expect[Map[Pos, Char]](uri"http://localhost:1225/grid")
      .flatMap(showGrid)

  private val frames: IO[Unit] =
    val request = WSRequest(uri"ws://localhost:1225/frames")
    WebSocketClient[IO].connectHighLevel(request).use:
      _.receiveStream // TODO: Is there an alternate method that handles decoding?
        .metered(20.milliseconds)
        .map(decodeFrame)
        .unNone
        .evalTap(showFrame)
        .compile
        .drain

  private val posRegex = """\((\d+),(\d+)\)""".r
  given KeyDecoder[Pos] with
    def apply(key: String): Option[Pos] =
      key match
        case posRegex(row, col) => Some(row.toInt, col.toInt)
        case _ => None
  given EntityDecoder[IO, Map[Pos, Char]] = jsonOf[IO, Map[Pos, Char]]

  // TODO: Integrate better with circe and generalize.
  private def decodeFrame(frame: WSDataFrame): Option[Frame] = frame match
    case WSFrame.Text(text, _) => parse(text).flatMap(_.as[Frame]).toOption
    case _ => None

  private def showFrame(frame: Frame): IO[Unit] =
    val changes = frame.changes.flatMap{case ((row, col), changes) =>
      val node = document.getElementById(s"$row,$col").asInstanceOf[Element]
      changes.map:
        case ChangeChar(char)       => IO(node.innerHTML = char.toString)
        case ChangeColor(color)     => IO(node.style.color = color)
        case ChangeTooltip(tooltip) => IO.unit // TODO: Implement tooltips
    }
    Parallel.parSequence(changes.toList).void

  private def showGrid(grid: Map[Pos, Char]): IO[Unit] =
    val minRow = grid.keySet.map(_._1).min
    val maxRow = grid.keySet.map(_._1).max
    val minCol = grid.keySet.map(_._2).min
    val maxCol = grid.keySet.map(_._2).max

    val table = (minRow to maxRow).map{row =>
      (minCol to maxCol).map{col =>
        s"<td id='$row,$col' style='height: 1em; width: 1em'>${grid(row -> col)}</td>"
      }.mkString
    }.mkString("<tr>", "</tr>\n<tr>", "</tr>\n")

    IO(document.querySelector("#grid").innerHTML = s"<table>$table</table>")
