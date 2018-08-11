package stork

import cats.Applicative
import cats.effect.IO
import org.http4s.{EntityDecoder, EntityEncoder, HeaderKey, Method, QueryParamDecoder, QueryParamEncoder}
import shapeless.HList

import scala.language.higherKinds

object dsl {
  def path[A: URLFormat]: PathItem[IO, A] = PathItem[IO, A]()

  def path(part: String): PathSegment[IO] = PathSegment(part)

  def param[A: QueryParamDecoder : QueryParamEncoder](name: String): QueryParam[IO, A] = QueryParam(name)

  def header[H <: HeaderKey.Extractable](key: H): Header[IO, H] = Header(key)

  def body[A](implicit decoder: EntityDecoder[IO, A], encoder: EntityEncoder[IO, A]): Body[IO, A] = Body()

  def get[F[_] : Applicative, H <: HList](route: Route[F, H]): Endpoint[F, H] =
    route.seal(Method.GET)

  def post[F[_] : Applicative, H <: HList](route: Route[F, H]): Endpoint[F, H] =
    route.seal(Method.POST)

  def put[F[_] : Applicative, H <: HList](route: Route[F, H]): Endpoint[F, H] =
    route.seal(Method.PUT)
}
