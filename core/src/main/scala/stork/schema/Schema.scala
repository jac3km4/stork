package stork.schema

import org.http4s.MediaType

final case class Schema(path: List[String], parameters: List[Parameter]) {
  def ++(other: Schema): Schema =
    Schema(path ++ other.path, parameters ++ other.parameters)
}

sealed trait Parameter

object Parameter {
  final case class Query(name: String, dataType: DataType, required: Boolean) extends Parameter
  final case class Body(dataType: DataType, mediaType: Option[MediaType], required: Boolean) extends Parameter
  final case class Path(name: String, dataType: DataType) extends Parameter
  final case class Header(name: String) extends Parameter
}

sealed trait DataType
object DataType {
  case object String extends DataType
  case object Number extends DataType
  case object Integer extends DataType
  case object Boolean extends DataType
  final case class Array(of: DataType) extends DataType
  final case class Object(members: Map[String, DataType]) extends DataType
}
