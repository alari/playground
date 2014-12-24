name := "scalaz-test"

version := "1.0"

mainClass := Some("learn.shared.SharedMain")

scalaVersion := "2.11.4"

val scalazV = "7.0.6"
val akkaV = "2.3.8"
val akkaHttpV = "1.0-M2"

resolvers += ("jdgoldie at bintray" at "http://dl.bintray.com/jdgoldie/maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-http-experimental_2.11" % akkaHttpV,
  "com.typesafe.akka" %% "akka-actor" % akkaV,
  "com.typesafe.akka" %% "akka-cluster" % akkaV,
  "com.typesafe.akka" %% "akka-contrib" % akkaV,
  "com.typesafe.akka" %% "akka-testkit" % akkaV % "test",
  "com.typesafe.akka" %% "akka-slf4j" % akkaV,
  "org.scalaz" %% "scalaz-core" % scalazV,
  "org.scalaz" %% "scalaz-effect" % scalazV,
  "org.scalaz" %% "scalaz-typelevel" % scalazV,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazV % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Ywarn-dead-code",
  "-language:_",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
  )

initialCommands in console := "import scalaz._, Scalaz._"

libraryDependencies += "joda-time" % "joda-time" % "2.6"