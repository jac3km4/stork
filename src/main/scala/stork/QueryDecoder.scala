package stork

trait QueryDecoder[A] {
  def decode(value: String): A
}

object QueryDecoder {
  implicit val stringDecoder: QueryDecoder[String] = value => value
  implicit val intDecoder: QueryDecoder[Int] = Integer.parseInt
}

trait URLDecoder[A] {
  def decode(value: String): A
}

object URLDecoder {
  implicit val stringDecoder: URLDecoder[String] = value => value
  implicit val intDecoder: URLDecoder[Int] = Integer.parseInt
}