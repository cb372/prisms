scalaVersion := "2.12.1"
scalaOrganization := "org.typelevel"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

//scalacOptions ++= Seq("-Ydebug", "-Xdev", "-Ylog:typer")
