ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "MyApp"
  )

libraryDependencies ++= {
  Seq(
    "org.json4s" %% "json4s-jackson" % "4.0.3",
    "com.google.code.gson" % "gson" % "2.8.2"
  )
}