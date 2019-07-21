inThisBuild(
  List(
    organization := "com.johnclara",
    licenses += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"),
    startYear := Some(2019),
    scalaVersion := "2.12.8",
    organizationName := "John Clara"
  )
)

val jsonnetCaseClass = project
  .in(file("."))
  .aggregate(core)
  .settings(
    publish / skip := true
  )

lazy val core = project
.in(file("core"))
.settings(
  name := "jsonnet-case-class",
  parallelExecution in Test := false
)
