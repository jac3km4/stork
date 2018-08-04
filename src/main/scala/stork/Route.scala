package stork

import cats.Applicative
import org.http4s.{HttpService, Method, Request, Response}
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
      override def apply(request: Request[F], value: A): Request[F] =
        self.apply(request.withMethod(method), value)

      override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[A]] =
        if (request.method == method) self.extract(request, remainingPath)
        else None
    }
  }

  def apply(request: Request[F], value: A): Request[F]
  def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[A]]
}

final case class Endpoint[F[_], In <: HList](route: Route[F, In]) extends AnyVal {
  def request[FN](base: String)
                (implicit fp: FnFromProduct.Aux[In => Request[F], FN]): FN =
    fp.apply(route.apply(Request[F](), _))

  def service[FN](f: FN)(implicit ap: Applicative[F],
                         fp: FnToProduct.Aux[FN, In => F[Response[F]]]): HttpService[F] = {
    val fn: Request[F] => Option[F[Response[F]]] = { req: Request[F] =>
      route.extract(req, req.uri.path.stripPrefix("/").split('/').toList).map { state =>
        fp(f)(state.value)
      }
    }
    HttpService(Function.unlift(fn))
  }
}

final case class PathSegment[F[_]: Applicative](value: String) extends Route[F, HNil] {
  override def apply(request: Request[F], hnil: HNil): Request[F] =
    request.withUri(request.uri / value)

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[HNil]] =
    remainingPath match {
      case `value` ::/ tail => Some(ExtractState(HNil, tail))
      case _ => None
    }
}

final case class PathItem[F[_]: Applicative, A: URLDecoder]() extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): Request[F] = {
    request.withUri(request.uri / value.head.toString) // FIXME
  }
  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[A :: HNil]] =
    remainingPath match {
      case head ::/ tail => Some(ExtractState(implicitly[URLDecoder[A]].decode(head):: HNil, tail))
      case _ => None
    }
}

final case class QueryParam[F[_]: Applicative, A: QueryDecoder](name: String) extends Route[F, A :: HNil] {
  override def apply(request: Request[F], value: A :: HNil): Request[F] = {
    request.withUri(request.uri.withQueryParam(name, value.head.toString)) // FIXME
  }

  override def extract(request: Request[F], remainingPath: List[String]): Option[ExtractState[A :: HNil]] = {
    request.params.get(name)
      .map(implicitly[QueryDecoder[A]].decode)
      .map(v => ExtractState(v :: HNil, remainingPath))
  }
}

final case class ExtractState[A](value: A, remainingParams: List[String])
