# stork
when shapeless meets http4s

#
Stork let's you define services in a way that let's you both define and call them in a type-safe way.
The correctness of request parameters is checked at compile-time and the implementation is IDE-friendly (IntelliJ will suggest the parameter types you need to pass to a request).

```scala
val endpoint = get(path("add") :: path[Int] :: path("and") :: param[Int]("this"))

val service = endpoint.service { (a: Int, b: Int) => Ok((a + b).toString) }

val request = endpoint.request("localhost").apply(1, 2)

println(service.run(request).value)
```
