package stork

import cats.Applicative
import cats.effect.IO
import org.http4s.Method
import shapeless.HList

import scala.language.higherKinds

object dsl {
  def path[A: URLDecoder]: PathItem[IO, A] = PathItem[IO, A]()

  def path(part: String): PathSegment[IO] = PathSegment(part)

  def param[A: QueryDecoder](name: String): QueryParam[IO, A] = QueryParam(name)

  def get[F[_]: Applicative, H <: HList](route: Route[F, H]): Endpoint[F, H] =
    route.seal(Method.GET)

  def post[F[_]: Applicative, H <: HList](route: Route[F, H]): Endpoint[F, H] =
    route.seal(Method.POST)
}
