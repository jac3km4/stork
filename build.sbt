name := "stork"

version := "0.1"

scalaVersion := "2.12.6"

val http4sVersion = "0.18.15"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
)

scalacOptions ++= Seq("-Ypartial-unification")
