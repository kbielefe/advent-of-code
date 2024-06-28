package visualizations

import cats.effect.*
import cats.effect.std.Console
import cats.effect.unsafe.IORuntime
import com.comcast.ip4s.*
import java.awt.Desktop
import java.net.URI
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.headers.`Content-Type`
import org.http4s.server.Router
import scala.concurrent.duration.*

object Browse:
  given IORuntime =
    cats.effect.unsafe.IORuntime.global

  def apply(html: String): Unit =
    val service = HttpRoutes.of[IO] {
      case GET -> Root =>
        Ok(html).map(_.withContentType(`Content-Type`(MediaType.text.html)))
    }
    val httpApp = Router("/" -> service).orNotFound
    val uri = URI.create("http://localhost:1225")
    val server = EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"1225")
      .withHttpApp(httpApp)
      .withShutdownTimeout(1.second)
      .build
      .use{_ =>
        IO(Desktop.getDesktop.browse(uri)) >>
        Console[IO].println("Press <enter> to stop web server") >>
        Console[IO].readLine
      }.unsafeRunSync()
