package stork

import cats.effect.IO
import cats.implicits._
import org.http4s.Request
import shapeless.{::, HList, HNil}
import stork.schema.Schema

trait Joiner[T1, T2] {
  type Out

  def join(t1: T1, t2: T2): Out
}

trait LowPrioJoiner {
  implicit def hnilJoiner[A <: HList]: Joiner.Aux[Route[IO, HNil], Route[IO, A], Route[IO, A]] =
    new Joiner[Route[IO, HNil], Route[IO, A]] {
      override type Out = Route[IO, A]

      override def join(t1: Route[IO, HNil], t2: Route[IO, A]): Out = new Route[IO, A] {
        override def apply(request: Request[IO], value: A): IO[Request[IO]] =
          t1.apply(request, HNil).flatMap(t2.apply(_, value))

        override def extract(request: Request[IO], remainingPath: List[String]): Option[ExtractState[IO, A]] =
          t1.extract(request, remainingPath).flatMap(st => t2.extract(request, st.remainingParams))

        override def schema: Schema = t1.schema ++ t2.schema
      }
    }
}

object Joiner extends LowPrioJoiner {
  type Aux[T1, T2, T3] = Joiner[T1, T2] {
    type Out = T3
  }

  implicit def mergeJoiner[A]: Joiner.Aux[Route[IO, HNil], Route[IO, A :: HNil], Route[IO, A :: HNil]] =
    new Joiner[Route[IO, HNil], Route[IO, A :: HNil]] {
      override type Out = Route[IO, A :: HNil]

      override def join(t1: Route[IO, HNil], t2: Route[IO, A :: HNil]): Out = new Route[IO, A :: HNil] {
        override def apply(request: Request[IO], value: A :: HNil): IO[Request[IO]] =
          t1.apply(request, HNil).flatMap(t2.apply(_, value))

        override def extract(request: Request[IO], remainingPath: List[String]): Option[ExtractState[IO, A :: HNil]] =
          t1.extract(request, remainingPath).flatMap(st => t2.extract(request, st.remainingParams))

        override def schema: Schema = t1.schema ++ t2.schema
      }
    }

  implicit def prependJoiner[A <: HList, B]: Joiner.Aux[Route[IO, B :: HNil], Route[IO, A], Route[IO, B :: A]] =
    new Joiner[Route[IO, B :: HNil], Route[IO, A]] {
      override type Out = Route[IO, B :: A]

      override def join(t1: Route[IO, B :: HNil], t2: Route[IO, A]): Out = new Route[IO, B :: A] {
        override def apply(request: Request[IO], value: B :: A): IO[Request[IO]] =
          t1.apply(request, value.head :: HNil).flatMap(t2.apply(_, value.tail))

        override def extract(request: Request[IO], remainingPath: List[String]): Option[ExtractState[IO, B :: A]] =
          for {
            st1 <- t1.extract(request, remainingPath)
            st2 <- t2.extract(request, st1.remainingParams)
          } yield ExtractState((st1.value, st2.value).mapN { (a, b) => a.head :: b }, st2.remainingParams)

        override def schema: Schema = t1.schema ++ t2.schema
      }
    }

}
