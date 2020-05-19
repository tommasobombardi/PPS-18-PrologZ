name := "PPS-18-PrologZ"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions += "language:implicitConversions"
scalacOptions += "language:postfixOps"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.30"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test"
