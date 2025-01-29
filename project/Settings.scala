import sbt.Keys.*

object Settings {
  val common = Seq(
    organization := "io.github.leviysoft",
    scalaVersion := "3.3.5",
    scalacOptions ++= Seq(
      // For reference: https://docs.scala-lang.org/scala3/guides/migration/options-lookup.html
      "-encoding",
      "utf8",
      "-deprecation",
      "-explain-types",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Xtarget:11",
      "-unchecked",
      "-Ykind-projector",
      "-Xcheck-macros",
      "-Yretain-trees",
      "-Wunused:all"
    ),
    semanticdbEnabled := true
  )
}
