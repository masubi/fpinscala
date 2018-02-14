val commonSettings = Seq(
  scalaVersion := "2.12.1",
  scalacOptions += "-Ypartial-unification",
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1",
  libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.1"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
