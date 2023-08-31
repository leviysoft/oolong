//To use Scalafix on Scala 3 projects, you must unset `scalafixBinaryScalaVersion`
//ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)

inThisBuild(
  List(
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    organization := "io.github.leviysoft",
    homepage := Some(url("https://github.com/leviysoft/oolong")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "edubrovski",
        "Eduard Dubrovsky",
        "dubrovskieddie@gmail.com",
        url("https://github.com/edubrovski")
      ),
      Developer(
        "danslapman",
        "Daniil Smirnov",
        "danslapman@gmail.com",
        url("https://github.com/danslapman")
      ),
      Developer(
        "desavitsky",
        "Denis Savitsky",
        "-",
        url("https://github.com/desavitsky")
      ),
      Developer(
        "Assassin4791",
        "Assassin4791",
        "-",
        url("https://github.com/Assassin4791")
      ),
      Developer(
        "InversionSpaces",
        "InversionSpaces",
        "InversionSpaces@vivaldi.net",
        url("https://github.com/InversionSpaces")
      )
    )
  )
)

val `oolong-bson` = (project in file("oolong-bson"))
  .settings(Settings.common)
  .settings(
    libraryDependencies ++= Seq(
      ("org.mongodb.scala"           %% "mongo-scala-bson" % "4.6.1").cross(CrossVersion.for3Use2_13),
      "com.softwaremill.magnolia1_3" %% "magnolia"         % "1.1.4",
      "org.scalatest"                %% "scalatest"        % "3.2.12"   % Test,
      "org.scalatestplus"            %% "scalacheck-1-16"  % "3.2.12.0" % Test,
      "org.scalacheck"               %% "scalacheck"       % "1.16.0"   % Test
    ),
    Test / fork := true,
  )

val `oolong-core` = (project in file("oolong-core"))
  .settings(Settings.common)
  .dependsOn(`oolong-bson`)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "pprint"    % "0.7.3"  % Compile,
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
    ),
    Test / fork := true
  )

val `oolong-mongo` = (project in file("oolong-mongo"))
  .settings(Settings.common)
  .dependsOn(`oolong-core`, `oolong-bson`)
  .settings(
    libraryDependencies ++= Seq(
      ("org.mongodb.scala" %% "mongo-scala-driver"             % "4.6.1"  % Test).cross(CrossVersion.for3Use2_13),
      "com.dimafeng"       %% "testcontainers-scala-scalatest" % "0.40.8" % Test,
      "com.dimafeng"       %% "testcontainers-scala-mongodb"   % "0.40.8" % Test,
      "org.scalatest"      %% "scalatest"                      % "3.2.12" % Test,
      "org.slf4j"           % "slf4j-api"                      % "1.7.36" % Test,
      "org.slf4j"           % "slf4j-simple"                   % "1.7.36" % Test,
    ),
    Test / fork := true
  )

val root = (project in file("."))
  .settings(Settings.common)
  .aggregate(`oolong-bson`, `oolong-core`, `oolong-mongo`)
  .settings(
    pullRemoteCache := {},
    pushRemoteCache := {},
    publish / skip := true
  )
  .settings(
    addCommandAlias(
      "fixCheck",
      "scalafixAll --check; scalafmtCheck"
    ),
    addCommandAlias(
      "lintAll",
      "scalafixAll; scalafmtAll"
    )
  )
