package stork.schema

import java.util.UUID

import org.http4s.Status
import shapeless.labelled.KeyTag
import shapeless.tag.Tagged
import stork.internal.Exported

trait SchemaDefinition[T] {
  def dataType: DataType
}

trait ResponseDefinition[T] {
  def statusCode: Status
  def dataType: DataType
}

object SchemaDefinition extends LowPrioSchemaDefinition {
  type FieldType[K, V] = Symbol with Tagged[K] with KeyTag[V, Symbol with Tagged[K]]

  def apply[T](dt: DataType): SchemaDefinition[T] = new SchemaDefinition[T] {
    override def dataType: DataType = dt
  }

  def apply[T: SchemaDefinition]: SchemaDefinition[T] =
    implicitly[SchemaDefinition[T]]

  implicit val stringSchemaDefinition: SchemaDefinition[String] = SchemaDefinition(DataType.String)
  implicit val uuidSchemaDefinition: SchemaDefinition[UUID]     = SchemaDefinition(DataType.String)
  implicit val intSchemaDefinition: SchemaDefinition[Int]       = SchemaDefinition(DataType.Integer)
  implicit val doubleSchemaDefinition: SchemaDefinition[Double] = SchemaDefinition(DataType.Number)
  implicit val boolSchemaDefinition: SchemaDefinition[Boolean]  = SchemaDefinition(DataType.Boolean)

  implicit def optionSchemaDefinition[A](implicit memberDefinition: SchemaDefinition[A]): SchemaDefinition[Option[A]] =
    SchemaDefinition(DataType.Optional(memberDefinition.dataType))

  implicit def listSchemaDefinition[A](implicit memberDefinition: SchemaDefinition[A]): SchemaDefinition[List[A]] =
    SchemaDefinition(DataType.Array(memberDefinition.dataType))

  private[stork] def prettyClassName(clazz: Class[_]): String = {
    val name = clazz.getSimpleName
    if (name.forall(c => !c.isLetter || c.isUpper))
      name.toLowerCase
    else name.charAt(0).toLower + name.substring(1)
  }
}

private[stork] trait LowPrioSchemaDefinition {
  implicit def importedSchemaDefinition[A](implicit exported: Exported[SchemaDefinition[A]]): SchemaDefinition[A] =
    exported.instance
}

object ResponseDefinition {
  implicit def importedResponseDefinition[A](implicit exported: Exported[ResponseDefinition[A]]): ResponseDefinition[A] =
    exported.instance
}
