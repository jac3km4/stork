package stork.schema

import cats.data.EitherT
import cats.{Applicative, Functor}
import org.http4s.{DecodeResult, EntityDecoder, EntityEncoder, Response, Status}
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.ops.record.SwapRecord
import shapeless.{HList, LabelledGeneric, Poly1}
import stork.internal.Exported
import stork.response.ResponseCodec
import stork.schema.SchemaDefinition.FieldType

object auto {
  private object derivePoly extends Poly1 {
    implicit def default[K, V: SchemaDefinition]: Case.Aux[FieldType[K, V], (String, DataType)] =
      at[FieldType[K, V]](sym => sym.name -> SchemaDefinition[V].dataType)
  }

  implicit def deriveClass[A, G <: HList, F <: HList, D <: HList](
      implicit lgen: LabelledGeneric.Aux[A, G],
      swapRecord: SwapRecord.Aux[G, F],
      mapper: Mapper.Aux[derivePoly.type, F, D],
      toTraversable: ToTraversable.Aux[D, List, (String, DataType)]
  ): Exported[SchemaDefinition[A]] = Exported {
    val fields  = swapRecord()
    val members = fields.map(derivePoly).toList.toMap
    SchemaDefinition(DataType.Object(members))
  }

  implicit val unitResponse: ResponseDefinition[Unit] = new ResponseDefinition[Unit] {
    override def statusCode: Status = Status.NoContent
    override def dataType: DataType = DataType.Null
  }

  implicit def schemaDefinitionResponse[A](implicit schemaDefinition: SchemaDefinition[A]): Exported[ResponseDefinition[A]] =
    Exported {
      new ResponseDefinition[A] {
        override def statusCode: Status = Status.Ok
        override def dataType: DataType = SchemaDefinition[A].dataType
      }
    }

  implicit def unitCodec[F[_]](implicit F: Applicative[F]): ResponseCodec[F, Unit] = new ResponseCodec[F, Unit] {
    override def from(response: Response[F]): DecodeResult[F, Unit] = EitherT.rightT(())
    override def to(t: Unit): F[Response[F]]                        = F.pure(Response[F](status = Status.NoContent))
  }

  implicit def encoderDecoderCodec[F[_], A](implicit F: Functor[F],
                                            encoder: EntityEncoder[F, A],
                                            decode: EntityDecoder[F, A]): ResponseCodec[F, A] = new ResponseCodec[F, A] {
    override def from(response: Response[F]): DecodeResult[F, A] = decode.decode(response, strict = false)
    override def to(t: A): F[Response[F]] = F.map(encoder.toEntity(t)) { entity =>
      Response[F](status = Status.Ok, body = entity.body)
    }
  }
}
