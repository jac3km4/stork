package stork

import java.util.UUID

import cats.Invariant
import cats.implicits._

trait URLFormat[A] {
  def decode(value: String): Option[A]

  def encode(value: A): String
}

object URLFormat {
  def apply[A](implicit format: URLFormat[A]): URLFormat[A] = format

  implicit val formatContravariant: Invariant[URLFormat] = new Invariant[URLFormat] {
    override def imap[A, B](fa: URLFormat[A])(f: A => B)(g: B => A): URLFormat[B] = new URLFormat[B] {
      override def decode(value: String): Option[B] = fa.decode(value).map(f)

      override def encode(value: B): String = fa.encode(g(value))
    }
  }

  implicit val stringFormat: URLFormat[String] = new URLFormat[String] {
    override def decode(value: String): Option[String] = Some(value)

    override def encode(value: String): String = value
  }

  implicit val intFormat: URLFormat[Int] = new URLFormat[Int] {
    override def decode(value: String): Option[Int] = Either.catchNonFatal(Integer.parseInt(value)).toOption

    override def encode(value: Int): String = value.toString
  }

  implicit val uuidFormat: URLFormat[UUID] = stringFormat.imap(UUID.fromString)(_.toString)
}
