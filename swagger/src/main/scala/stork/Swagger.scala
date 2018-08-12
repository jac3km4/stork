package stork

import io.circe.syntax._
import io.circe.{Encoder, Json}
import shapeless.HList
import stork.schema.{DataType, Parameter, ResponseDefinition}

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
      val required = members.filterNot {
        case (_, DataType.Optional(_)) => true
        case _ => false
      }.map { case (key, _) => key }

      Json.obj("type" -> "object".asJson, "properties" -> members.asJson, "required" -> required.asJson)
    case DataType.Array(of) =>
      Json.obj("type" -> "array".asJson, "items" -> of.asJson)
    case DataType.Optional(member) => member.asJson
    case DataType.String  => Json.obj("type" -> "string".asJson)
    case DataType.Integer => Json.obj("type" -> "integer".asJson)
    case DataType.Number  => Json.obj("type" -> "number".asJson)
    case DataType.Boolean => Json.obj("type" -> "boolean".asJson)
    case DataType.Null    => Json.Null
  }

  implicit def parameterEncoder: Encoder[Parameter] = Encoder.instance {
    case Parameter.Body(dataType, mediaType, required) =>
      Json
        .obj(
          "in"       -> "body".asJson,
          "name"     -> "body".asJson,
          "required" -> required.asJson
        )
        .deepMerge(encodeSchema(dataType))
    case Parameter.Header(name) =>
      Json.obj(
        "in"   -> "header".asJson,
        "name" -> name.asJson,
        "type" -> "string".asJson
      )
    case Parameter.Path(name, dataType) =>
      Json
        .obj(
          "in"       -> "path".asJson,
          "name"     -> name.asJson,
          "required" -> true.asJson
        )
        .deepMerge(encodeSchema(dataType))
    case Parameter.Query(name, dataType, required) =>
      Json
        .obj(
          "in"       -> "query".asJson,
          "name"     -> name.asJson,
          "required" -> required.asJson
        )
        .deepMerge(encodeSchema(dataType))
  }

  implicit def endpointEncoder[F[_], H <: HList, Out](
      implicit response: ResponseDefinition[Out]
  ): Encoder[TypedEndpoint[F, H, Out]] =
    Encoder.instance { endpoint =>
      val schema = endpoint.route.schema
      Json.obj(
        s"/${schema.path.mkString("/")}" -> Json.obj(
          endpoint.method.name.toLowerCase -> Json.obj(
            "parameters" -> schema.parameters.asJson,
            "responses" -> Json.obj(
              response.statusCode.code.toString ->
                encodeSchema(response.dataType)
                  .deepMerge(Json.obj("description" -> "Successful response".asJson))
            )
          )
        )
      )
    }

  def generate[F[_], H <: HList, Out: ResponseDefinition](endpoint: TypedEndpoint[F, H, Out]): Json =
    endpointEncoder.apply(endpoint)
}
