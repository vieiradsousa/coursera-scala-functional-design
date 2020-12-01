name := "coursera-scala-functional-design"

version := "0.1"

scalaVersion := "2.13.4"

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"
libraryDependencies += "org.typelevel" %% "jawn-parser" % "0.14.2"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
