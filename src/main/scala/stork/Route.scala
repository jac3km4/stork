package stork

import cats.implicits._
import cats.{Applicative, Monad}
import org.http4s.{EntityDecoder, EntityEncoder, HeaderKey, HttpService, Method, QueryParamDecoder, QueryParamEncoder, QueryParameterValue, Request, Response, Uri}
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.{::, HList, HNil}

import scala.language.higherKinds
import scala.{:: => ::/}

private[stork] trait Route[F[_], A <: HList] {
  self =>

  final def ::[B <: HList, C <: HList](other: Route[F, B])
                                      (implicit join: Joiner.Aux[Route[F, B], Route[F, A], Route[F, C]]): Route[F, C] =
    join.join(other, self)

  final def seal(method: Method)(implicit F: Applicative[F]): Endpoint[F, A] = Endpoint {
    new Route[F, A] {
      override def apply(request: Request[F], value: A): F[Request[F]] =
        self.apply(request.withMethod(method), value)

      override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A]] =
        if (request.method == method) self.extract(request, remainingPath)
        else None
    }
  }

  def apply(request: Request[F], value: A): F[Request[F]]

  def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A]]
}

final case class Endpoint[F[_], In <: HList](route: Route[F, In]) extends AnyVal {
  def request[FN](base: String)
                 (implicit fp: FnFromProduct.Aux[In => F[Request[F]], FN]): FN =
    fp.apply(route.apply(Request[F](uri = Uri.unsafeFromString(base)), _))

  def service[FN](f: FN)(implicit F: Monad[F],
                         fp: FnToProduct.Aux[FN, In => F[Response[F]]]): HttpService[F] = {
    val fn: Request[F] => Option[F[Response[F]]] = { req =>
      route.extract(req, req.uri.path.stripPrefix("/").split('/').toList).map { state =>
        state.value.flatMap(fp(f))
      }
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
      case _ => None
    }
}

final case class PathItem[F[_], A: URLFormat](implicit F: Applicative[F]) extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): F[Request[F]] =
    F.pure(request.withUri(request.uri / URLFormat[A].encode(value.head)))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A :: HNil]] =
    remainingPath match {
      case head ::/ tail =>
        URLFormat[A].decode(head).map(v => ExtractState(F.pure(v :: HNil), tail))
      case _ => None
    }
}

final case class QueryParam[F[_], A: QueryParamDecoder : QueryParamEncoder](name: String)(implicit F: Applicative[F]) extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): F[Request[F]] =
    F.pure(request.withUri(request.uri.withQueryParam(name, QueryParamEncoder[A].encode(value.head).value)))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A :: HNil]] = for {
    value <- request.params.get(name)
    decoded <- QueryParamDecoder[A].decode(QueryParameterValue(value)).toOption
  } yield ExtractState(F.pure(decoded :: HNil), remainingPath)
}

final case class Header[F[_], H <: HeaderKey.Extractable](key: H)(implicit F: Applicative[F]) extends Route[F, H#HeaderT :: HNil] {
  override def apply(request: Request[F], value: H#HeaderT :: HNil): F[Request[F]] =
    F.pure(request.putHeaders(value.head))

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, H#HeaderT :: HNil]] =
    request.headers.get(key).map(v => ExtractState(F.pure(v :: HNil), remainingPath))
}

final case class Body[F[_], A](implicit F: Monad[F],
                               encoder: EntityEncoder[F, A],
                               decoder: EntityDecoder[F, A]) extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): F[Request[F]] =
    request.withBody(value.head)

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[F, A :: HNil]] = {
    val body = request.as[A]
    Some(ExtractState(body.map(_ :: HNil), remainingPath))
  }
}

final case class ExtractState[F[_], A](value: F[A], remainingParams: List[String])
