package stork.response

import org.http4s.{DecodeResult, Response}
import stork.internal.Exported

trait ResponseCodec[F[_], T] {
  def from(response: Response[F]): DecodeResult[F, T]
  def to(t: T): F[Response[F]]
}

object ResponseCodec {
  implicit def importedResponseCodec[F[_], A](implicit exported: Exported[ResponseCodec[F, A]]): ResponseCodec[F, A] =
    exported.instance
}
