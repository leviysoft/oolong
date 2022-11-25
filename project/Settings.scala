import scalafix.sbt.ScalafixPlugin.autoImport.scalafixResolvers

import coursierapi.{MavenRepository => CoursierMvnRepo}
import sbt.Keys._
import sbt._

object Settings {
  val common = Seq(
    organization := "ru.tinkoff",
    version := "0.1",
    scalaVersion := "3.1.3",
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
      "-Yretain-trees"
    ),
    semanticdbEnabled := true
  )
}
