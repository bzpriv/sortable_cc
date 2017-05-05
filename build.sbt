name := """sortable_challenge"""

version := "1.0"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.github.pathikrit" % "better-files_2.11" % "2.13.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalaz" %% "scalaz-core" % "7.2.11",
  "io.argonaut" %% "argonaut" % "6.2",
  "com.github.alexarchambault" %% "argonaut-shapeless_6.2" % "1.2.0-M4")

