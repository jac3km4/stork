package stork

import cats.implicits._
import cats.{Applicative, Monad}
import org.http4s._
import org.http4s.client.Client
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.{::, HList, HNil}
import stork.internal.Joiner
import stork.response.ResponseCodec
import stork.schema.{Parameter, Schema, SchemaDefinition}

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.{:: => ::/}

private[stork] trait Route[F[_], A <: HList] {
  self =>

  final def ::[B <: HList, C <: HList](
      other: Route[F, B]
  )(implicit joiner: Joiner.Aux[Route[F, B], Route[F, A], Route[F, C]]): Route[F, C] =
    joiner.join(other, self)

  final def seal(method: Method)(implicit F: Applicative[F]): Endpoint[F, A] = new Endpoint(method, this)

  def apply(request: Request[F], value: A): F[Request[F]]

  def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A]]

  def schema: Schema
}

object Endpoint {
  private[stork] def request[F[_], A <: HList](method: Method, base: String, route: Route[F, A])(input: A): F[Request[F]] =
    route.apply(Request[F](method = method, uri = Uri.unsafeFromString(base)), input)

  private[stork] def extraction[F[_]: Monad, A <: HList](method: Method, route: Route[F, A])(req: Request[F]): Option[F[A]] =
    if (req.method == method) {
      route.extract(req, req.uri.path.stripPrefix("/").split('/').toList).map(_.value)
    } else None
}

final class Endpoint[F[_], In <: HList](val method: Method, val route: Route[F, In]) {
  def request[FN](base: String)(implicit fp: FnFromProduct.Aux[In => F[Request[F]], FN]): FN =
    fp.apply(Endpoint.request(method, base, route))

  def service[FN](f: FN)(implicit F: Monad[F], fp: FnToProduct.Aux[FN, In => F[Response[F]]]): HttpService[F] = {
    val fn = { req: Request[F] =>
      Endpoint.extraction(method, route)(req).map(_.flatMap(fp(f)))
    }
    HttpService(Function.unlift(fn))
  }

  def as[Out](implicit responseCodec: ResponseCodec[F, Out]): TypedEndpoint[F, In, Out] = new TypedEndpoint(method, route)
}

final class TypedEndpoint[F[_], In <: HList, Out](val method: Method, val route: Route[F, In])(
    implicit responseCodec: ResponseCodec[F, Out]
) {
  def request[FN](service: HttpService[F])(implicit F: Monad[F],
                                                         fp: FnFromProduct.Aux[In => F[Option[Out]], FN]): FN =
    fp.apply { params =>
      for {
        req <- Endpoint.request(method, "", route)(params)
        res <- service.run(req).value
        out <- res match {
          case None    => F.pure(None)
          case Some(v) => responseCodec.from(v).value.map(_.toOption)
        }
      } yield out
    }

  def request[FN](base: String, client: Client[F])(implicit F: Monad[F], fp: FnFromProduct.Aux[In => F[Either[DecodeFailure, Out]], FN]): FN =
    fp.apply { params =>
      client.fetch(Endpoint.request(method, base, route)(params))(responseCodec.from(_).value)
    }

  def service[FN](f: FN)(implicit F: Monad[F], fp: FnToProduct.Aux[FN, In => F[Out]]): HttpService[F] = {
    val fn = { req: Request[F] =>
      Endpoint.extraction(method, route)(req).map(_.flatMap(fp(f)).flatMap(responseCodec.to))
    }
    HttpService(Function.unlift(fn))
  }
}

final case class PathSegment[F[_]](value: String)(implicit F: Applicative[F]) extends Route[F, HNil] {
  override def apply(request: Request[F], hnil: HNil): F[Request[F]] =
    F.pure(request.withUri(request.uri / value))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, HNil]] =
    remainingPath match {
      case `value` ::/ tail => Some(ExtractState(F.pure(HNil), tail))
      case _                => None
    }

  override def schema: Schema = Schema(path = List(value), parameters = Nil)
}

final case class PathItem[F[_], A]()(implicit F: Applicative[F],
                                     ct: ClassTag[A],
                                     format: URLFormat[A],
                                     schemaDefinition: SchemaDefinition[A])
    extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): F[Request[F]] =
    F.pure(request.withUri(request.uri / URLFormat[A].encode(value.head)))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A :: HNil]] =
    remainingPath match {
      case head ::/ tail =>
        URLFormat[A].decode(head).map(v => ExtractState(F.pure(v :: HNil), tail))
      case _ => None
    }

  override def schema: Schema = {
    val name = SchemaDefinition.prettyClassName(ct.runtimeClass)
    Schema(path = List(s"{$name}"), parameters = List(Parameter.Path(name, SchemaDefinition[A].dataType)))
  }
}

final case class QueryParam[F[_], A](name: String)(implicit F: Applicative[F],
                                                   decoder: QueryParamDecoder[A],
                                                   encoder: QueryParamEncoder[A],
                                                   schemaDefinition: SchemaDefinition[A])
    extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): F[Request[F]] =
    F.pure(request.withUri(request.uri.withQueryParam(name, encoder.encode(value.head).value)))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A :: HNil]] =
    for {
      value   <- request.params.get(name)
      decoded <- decoder.decode(QueryParameterValue(value)).toOption
    } yield ExtractState(F.pure(decoded :: HNil), remainingPath)

  override def schema: Schema =
    Schema(Nil, List(Parameter.Query(name, SchemaDefinition[A].dataType, required = true)))
}

final case class OptQueryParam[F[_], A](name: String)(implicit F: Applicative[F],
                                                      decoder: QueryParamDecoder[A],
                                                      encoder: QueryParamEncoder[A],
                                                      schemaDefinition: SchemaDefinition[A])
    extends Route[F, Option[A] :: HNil] {
  override def apply(request: Request[F], value: Option[A] :: HNil): F[Request[F]] =
    F.pure(value.head.fold(request)(v => request.withUri(request.uri.withQueryParam(name, encoder.encode(v).value))))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, Option[A] :: HNil]] = {
    val result = for {
      value   <- request.params.get(name)
      decoded <- decoder.decode(QueryParameterValue(value)).toOption
    } yield decoded
    Some(ExtractState(F.pure(result :: HNil), remainingPath))
  }

  override def schema: Schema =
    Schema(Nil, List(Parameter.Query(name, SchemaDefinition[A].dataType, required = false)))
}

final case class Header[F[_], H <: HeaderKey.Extractable](key: H)(implicit F: Applicative[F])
    extends Route[F, H#HeaderT :: HNil] {
  override def apply(request: Request[F], value: H#HeaderT :: HNil): F[Request[F]] =
    F.pure(request.putHeaders(value.head))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, H#HeaderT :: HNil]] =
    request.headers.get(key).map(v => ExtractState(F.pure(v :: HNil), remainingPath))

  override def schema: Schema = Schema(Nil, List(Parameter.Header(key.name.value)))
}

final case class Body[F[_], A]()(implicit F: Monad[F],
                                 encoder: EntityEncoder[F, A],
                                 decoder: EntityDecoder[F, A],
                                 schemaDefinition: SchemaDefinition[A])
    extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): F[Request[F]] =
    request.withBody(value.head)

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A :: HNil]] = {
    val body = request.as[A]
    Some(ExtractState(body.map(_ :: HNil), remainingPath))
  }

  override def schema: Schema =
    Schema(Nil, List(Parameter.Body(schemaDefinition.dataType, encoder.contentType.map(_.mediaType), required = true)))
}

final case class ExtractState[F[_], A](value: F[A], remainingParams: List[String])
