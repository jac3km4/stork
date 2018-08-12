package stork

import java.util.UUID

import cats.effect.IO
import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.{EntityDecoder, EntityEncoder, HttpService}
import stork.dsl._
import stork.schema.auto._

object Main {
  final case class User(firstName: String, lastName: String, age: Int)

  implicit val encoder: EntityEncoder[IO, User] = jsonEncoderOf[IO, User]
  implicit val decoder: EntityDecoder[IO, User] = jsonOf[IO, User]

  val endpoint = put(path("users") :: path[UUID] :: body[User]).as[User]

  val service: HttpService[IO] = endpoint.service { (a: UUID, b: User) => IO.pure(b) }

  val request: IO[Option[User]] = endpoint.request(service).apply(UUID.randomUUID(), User("John", "Cena", 45))

  def main(args: Array[String]): Unit = {
    println(request.unsafeRunSync())
    println(Swagger.generate(endpoint))
  }
}
