import scala.sys.process.Process

name := "FPScala"

version := "1.0"
scalaVersion := "2.12.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.21"
libraryDependencies += "org.scalaz" %% "scalaz-ioeffect" % "2.0.0"
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

val gitHeadCommitSha = taskKey[String](
  "Determines the current git commit SHA")
gitHeadCommitSha := Process("git rev-parse HEAD").lines.head


