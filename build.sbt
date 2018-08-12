
val http4sVersion = "0.18.15"
val circeVersion = "0.9.3"

lazy val core = project
  .settings(
    moduleName := "stork-core",
    commonSettings,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion
    )
  )

lazy val swagger = project
  .settings(
    moduleName := "stork-swagger",
    commonSettings,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion
    )
  ).dependsOn(core)

lazy val example = project
  .settings(
    moduleName := "stork-example",
    commonSettings,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic" % circeVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion
    )
  ).dependsOn(swagger)

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation",
  "-encoding", "utf8",
  "-Ypartial-unification"
)

lazy val commonSettings = Seq(
  scalaVersion in ThisBuild := "2.12.6",
  scalacOptions ++= compilerOptions,
  version := "0.1"
)
