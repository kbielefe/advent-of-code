package visualizations

import cats.effect.IO
import cats.effect.std.Console
import fs2.{Pipe, Stream}
import io.circe.Encoder
import io.circe.syntax.*
import org.http4s.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame, WebSocketFrame.Text

trait Websocket:
  def build(builder: WebSocketBuilder2[IO]): IO[Response[IO]]

class SendOnlyWebsocket[A: Encoder](send: Stream[IO, A]) extends Websocket:
  override def build(builder: WebSocketBuilder2[IO]): IO[Response[IO]] =
    val sendFrame = send.map(_.asJson.deepDropNullValues.noSpaces).map(Text(_))
    val logReceive: Pipe[IO, WebSocketFrame, Unit] = _.evalMap(Console[IO].println)
    builder.build(sendFrame, logReceive)
