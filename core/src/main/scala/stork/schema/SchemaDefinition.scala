package stork.schema

import java.util.UUID

import shapeless._
import shapeless.labelled.KeyTag
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.ops.record.SwapRecord
import shapeless.tag.Tagged
import stork.schema.SchemaDefinition._

case class Exported[T](instance: T) extends AnyVal

trait SchemaDefinition[T] {
  def dataType: DataType
}

trait LowPrioImplicits {
  implicit def deriveClass[A, G <: HList, F <: HList, D <: HList](implicit lgen: LabelledGeneric.Aux[A, G],
                                                                  keys: SwapRecord.Aux[G, F],
                                                                  mapper: Mapper.Aux[derivePoly.type, F, D],
                                                                  toTraversable: ToTraversable.Aux[D, List, (String, DataType)]): SchemaDefinition[A] =
    SchemaDefinition(DataType.Object(SwapRecord[G].apply().map(derivePoly).toList.toMap))
}

object SchemaDefinition extends LowPrioImplicits {
  def apply[T](dt: DataType): SchemaDefinition[T] = new SchemaDefinition[T] {
    override def dataType: DataType = dt
  }

  def apply[T: SchemaDefinition]: SchemaDefinition[T] = implicitly[SchemaDefinition[T]]

  object derivePoly extends Poly1 {
    type FieldType[K, V] = Symbol with Tagged[K] with KeyTag[V, Symbol with Tagged[K]]

    implicit def default[K, V: SchemaDefinition]: Case.Aux[FieldType[K, V], (String, DataType)] =
      at[FieldType[K, V]](sym => sym.name -> SchemaDefinition[V].dataType)
  }

  implicit val stringSchemaDefinition: SchemaDefinition[String] = SchemaDefinition(DataType.String)
  implicit val uuidSchemaDefinition: SchemaDefinition[UUID] = SchemaDefinition(DataType.String)
  implicit val intSchemaDefinition: SchemaDefinition[Int] = SchemaDefinition(DataType.Integer)
  implicit val doubleSchemaDefinition: SchemaDefinition[Double] = SchemaDefinition(DataType.Number)
  implicit val boolSchemaDefinition: SchemaDefinition[Boolean] = SchemaDefinition(DataType.Boolean)

  implicit def listSchemaDefinition[A](list: List[A])(implicit memberDefinition: SchemaDefinition[A]): SchemaDefinition[List[A]] =
    SchemaDefinition(DataType.Array(memberDefinition.dataType))

  private[stork] def prettyClassName(clazz: Class[_]): String = {
    val name = clazz.getSimpleName
    if (name.forall(c => !c.isLetter || c.isUpper))
      name.toLowerCase
    else name.charAt(0).toLower + name.substring(1)
  }
}
