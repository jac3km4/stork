package stork

import io.circe.syntax._
import io.circe.{Encoder, Json}
import shapeless.HList
import stork.schema.{DataType, Parameter}

object Swagger {
  private def encodeSchema(dataType: DataType): Json = dataType match {
    case arr: DataType.Array =>
      Json.obj("schema" -> dataTypeEncoder(arr))
    case obj: DataType.Object =>
      Json.obj("schema" -> dataTypeEncoder(obj))
    case primitive => dataTypeEncoder(primitive)
  }

  implicit def dataTypeEncoder: Encoder[DataType] = Encoder.instance {
    case DataType.Object(members) =>
      Json.obj("type" -> "object".asJson, "properties" -> members.asJson)
    case DataType.Array(of) =>
      Json.obj("type" -> "array".asJson, "items" -> of.asJson)
    case DataType.String => Json.obj("type" -> "string".asJson)
    case DataType.Integer => Json.obj("type" -> "integer".asJson)
    case DataType.Number => Json.obj("type" -> "number".asJson)
    case DataType.Boolean => Json.obj("type" -> "boolean".asJson)
  }

  implicit def parameterEncoder: Encoder[Parameter] = Encoder.instance {
    case Parameter.Body(dataType, mediaType, required) =>
      Json.obj(
        "in" -> "body".asJson,
        "name" -> "body".asJson,
        "required" -> required.asJson
      ).deepMerge(encodeSchema(dataType))
    case Parameter.Header(name) =>
      Json.obj(
        "in" -> "header".asJson,
        "name" -> name.asJson,
        "type" -> "string".asJson
      )
    case Parameter.Path(name, dataType) =>
      Json.obj(
        "in" -> "path".asJson,
        "name" -> name.asJson,
        "required" -> true.asJson
      ).deepMerge(encodeSchema(dataType))
    case Parameter.Query(name, dataType, required) =>
      Json.obj(
        "in" -> "query".asJson,
        "name" -> name.asJson,
        "required" -> required.asJson
      ).deepMerge(encodeSchema(dataType))
  }

  implicit def endpointEncoder[F[_], H <: HList]: Encoder[Endpoint[F, H]] = Encoder.instance { endpoint =>
    val schema = endpoint.route.schema
    Json.obj(s"/${schema.path.mkString("/")}" -> Json.obj(
      endpoint.method.name.toLowerCase -> Json.obj(
        "parameters" -> schema.parameters.asJson,
        "responses" -> Json.obj(
          "200" -> Json.obj(
            "description" -> "abcd".asJson // FIXME
          )
        )
      )
    ))
  }

  def generate[F[_], H <: HList](endpoint: Endpoint[F, H]): Json = endpointEncoder(endpoint)
}
