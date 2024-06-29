package visualizations

import cats.effect.IO
import cats.effect.std.Console
import com.comcast.ip4s.*
import io.circe.Json
import java.awt.Desktop
import java.net.URI
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.headers.`Content-Type`
import org.http4s.server.Router
import org.http4s.server.middleware.*
import org.http4s.server.staticcontent.*
import org.http4s.server.websocket.WebSocketBuilder2
import scala.concurrent.duration.*

object Browse:
  def apply(html: String, websockets: Map[String, Websocket] = Map.empty, json: Map[String, Json] = Map.empty): IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"1225")
      .withHttpWebSocketApp(httpApp(html, websockets, json))
      .withShutdownTimeout(1.second)
      .build
      .use{_ =>
        IO(Desktop.getDesktop.browse(URI.create("http://localhost:1225"))) >>
        Console[IO].println("Press <enter> to stop web server") >>
        Console[IO].readLine
      }.void
  end apply

  private def service(html: String, websockets: Map[String, Websocket], json: Map[String, Json], builder: WebSocketBuilder2[IO]): HttpRoutes[IO] = HttpRoutes.of[IO]:
    case GET -> Root / path if json.contains(path) =>
      Ok(json(path)).map(_.withContentType(`Content-Type`(MediaType.application.json, Charset.`UTF-8`)))
    case GET -> Root / path if websockets.contains(path) =>
      websockets(path).build(builder)
    case GET -> Root / path if path.endsWith(".js") =>
      StaticFile.fromResource[IO]("/" + path).getOrElseF(NotFound()).map(_.withContentType(`Content-Type`(MediaType.application.javascript, Charset.`UTF-8`)))
    case GET -> Root =>
      Ok(html).map(_.withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`)))

  private def httpApp(html: String, websockets: Map[String, Websocket], json: Map[String, Json])(builder: WebSocketBuilder2[IO]): HttpApp[IO] =
    val corsService = CORS.policy.withAllowOriginHost(_.host.value == "localhost")(service(html, websockets, json, builder))
    Router("/" -> corsService).orNotFound
