# stork
when shapeless meets http4s

#
Stork allows you to create services in a way that let's you do both: define and call them in a type-safe way.
The correctness of service and request types is checked at compile-time and the implementation is IDE-friendly (IntelliJ will suggest the parameter types you need to pass to a request). Stork also allows you to generate detailed swagger definitions for your endpoints.

#
Example [from here](https://github.com/jac3km4/stork/blob/master/example/src/main/scala/stork/Main.scala)
```scala
  final case class User(firstName: String, lastName: String, age: Int)

  implicit val encoder: EntityEncoder[IO, User] = jsonEncoderOf[IO, User]
  implicit val decoder: EntityDecoder[IO, User] = jsonOf[IO, User]

  val endpoint = put(path("users") :: path[UUID] :: body[User]).as[User]

  val service: HttpService[IO] = endpoint.service { (a: UUID, b: User) => IO.pure(b) }

  val request: IO[Option[User]] = endpoint.request(service).apply(UUID.randomUUID(), User("John", "Cena", 45))

  println(request.unsafeRunSync()) // Some(User(John,Cena,45))
  println(Swagger.generate(endpoint))
```
