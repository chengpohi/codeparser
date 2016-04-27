name := "codeparser"


lazy val commonSettings = Seq(
  organization := "com.github.chengpohi",
  version := "0.1",
  scalaVersion := "2.11.7"
)

ivyScala := ivyScala.value map {
  _.copy(overrideScalaVersion = true)
}

val commonDependencies = Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.slf4j" % "slf4j-simple" % "1.7.7",
  "org.json4s" %% "json4s-native" % "3.2.10",
  "org.json4s" %% "json4s-jackson" % "3.2.10",
  "com.typesafe" % "config" % "1.2.1",
  "com.github.chengpohi" % "elasticshell_2.11" % "0.1"
)

lazy val codeparser = (project in file("."))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)
  .settings(
    assemblyJarName in assembly := "codeparser.jar"
  )
