name := "scalaz-test"

version := "1.0"

mainClass := Some("learn.storage.MainExp")

scalaVersion := "2.11.4"

val scalazV = "7.1.0"
val akkaV = "2.3.8"
val akkaHttpV = "1.0-M2"

resolvers += ("jdgoldie at bintray" at "http://dl.bintray.com/jdgoldie/maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-experimental" % akkaHttpV,
  "com.typesafe.akka" %% "akka-actor" % akkaV,
  "com.typesafe.akka" %% "akka-cluster" % akkaV,
  "com.typesafe.akka" %% "akka-contrib" % akkaV,
  "com.typesafe.akka" %% "akka-testkit" % akkaV % "test",
  "com.typesafe.akka" %% "akka-slf4j" % akkaV,
  "org.scalaz" %% "scalaz-core" % scalazV,
  "org.scalaz" %% "scalaz-effect" % scalazV,
  "org.scalaz" %% "scalaz-typelevel" % scalazV,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazV % "test",
  "org.specs2" %% "specs2-core" % "2.4.15" % "test",
  "org.pegdown" % "pegdown" % "1.4.2",
  "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1"
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

scalacOptions in Test ++= Seq("-Yrangepos")

initialCommands in console := "import scalaz._, Scalaz._"

libraryDependencies += "joda-time" % "joda-time" % "2.6"