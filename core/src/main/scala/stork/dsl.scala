package stork

import cats.effect.IO
import org.http4s.{EntityDecoder, EntityEncoder, HeaderKey, Method, QueryParamDecoder, QueryParamEncoder}
import shapeless.HList
import stork.schema.SchemaDefinition

import scala.language.higherKinds
import scala.reflect.ClassTag

object dsl {
  def path[A: ClassTag: URLFormat: SchemaDefinition]: PathItem[IO, A] = PathItem[IO, A]()

  def path(part: String): PathSegment[IO] = PathSegment(part)

  def param[A: QueryParamDecoder: QueryParamEncoder: SchemaDefinition](name: String): QueryParam[IO, A] = QueryParam(name)

  def paramOpt[A: QueryParamDecoder: QueryParamEncoder: SchemaDefinition](name: String): OptQueryParam[IO, A] =
    OptQueryParam(name)

  def header[H <: HeaderKey.Extractable](key: H): Header[IO, H] = Header(key)

  def body[A: SchemaDefinition](implicit decoder: EntityDecoder[IO, A], encoder: EntityEncoder[IO, A]): Body[IO, A] = Body()

  def get[H <: HList](route: Route[IO, H]): Endpoint[IO, H] =
    route.seal(Method.GET)

  def post[H <: HList](route: Route[IO, H]): Endpoint[IO, H] =
    route.seal(Method.POST)

  def put[H <: HList](route: Route[IO, H]): Endpoint[IO, H] =
    route.seal(Method.PUT)
}
