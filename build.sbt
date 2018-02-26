import Dependencies._
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.sbtplugin.cross.CrossProject
import sbtassembly.AssemblyPlugin.defaultShellScript

lazy val cmdlineProfile =
  sys.env.getOrElse("SBT_PROFILE", "")

def profile: CrossProject ⇒ CrossProject = pr => cmdlineProfile match {
  case "coverage" => pr.enablePlugins(ScoverageSbtPlugin)
  case _ => pr.disablePlugins(ScoverageSbtPlugin)
}

val DebugTest = config("dtest") extend Test

def scalacOptionsVersion(scalaVersion: String) = {
  Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
    "-Ypatmat-exhaust-depth", "40"
  ) ++ (CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 11)) => Nil
    case Some((2, 12)) => Seq(
      "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
      "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
      "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
      "-Xlint:option-implicit", // Option.apply used implicit view.
      "-Xlint:package-object-classes", // Class or object defined in package object.
      "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match", // Pattern match may not be typesafe.
      "-Ywarn-unused:privates", // Warn if a private member is unused.
      "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:params", // Warn if a value parameter is unused.
      "-Ywarn-unused:locals", // Warn if a local definition is unused.
      "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
      "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
      "-Ywarn-unused:implicits" // Warn if an implicit parameter is unused.
    )
    case _ => Nil
  })
}

lazy val root = project
  .in(file("."))
  .aggregate(rpm4sJS, rpm4sJVM, repoUtils, cli)
  .settings(
    coverageEnabled := false,
    coverageMinimum := 55, // TODO: increase this once we have more
    publishArtifact := false,
    publish := {},
    publishLocal := {}
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(
    coverageEnabled := false
  )
  .dependsOn(rpm4sJVM)
  .enablePlugins(JmhPlugin)

lazy val rpm4s = crossProject
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .configs(DebugTest)
  .settings(inConfig(DebugTest)(Defaults.testSettings): _*)
  .configureCross(profile)
  .settings(
    coverageEnabled := true,
    coverageFailOnMinimum := true,
    buildInfoPackage := "rpm4s",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    fork in DebugTest := true,
    javaOptions in DebugTest ++= Seq(
      "-Xdebug",
      "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=127.0.0.1:5005"
    ),
    definedTests in DebugTest := (definedTests in Test).value
  )
  .settings(
    organization := "io.lullabyte",
    scalaVersion := "2.12.4",
    crossScalaVersions := Seq("2.12.4"),
    name := "rpm4s",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % shapeless,
      "org.scalatest" %%% "scalatest" % scalatest,
      "org.scalacheck" %%% "scalacheck" % scalacheck,
      "org.typelevel" %%% "cats-core" % cats,
      "org.scodec" %% "scodec-core" % scodecCore
    ),
    scalacOptions ++= scalacOptionsVersion(scalaVersion.value),
    scalacOptions in (Compile, console) ~= (_.filterNot(
      Set(
        "-Ywarn-unused:imports",
        "-Xfatal-warnings"
      )))
  )

lazy val rpm4sJVM = rpm4s.jvm
  .settings(
    coverageMinimum := 65, // TODO: increase this once we have more
    libraryDependencies ++= Seq(
    )
  )

lazy val rpm4sJS = rpm4s.js
  .settings(
    coverageMinimum := 38, // TODO: increase this once we have more
    parallelExecution := false,
    jsEnv := new NodeJSEnv()
  )

lazy val cli = project.in(file("cli"))
  .dependsOn(rpm4sJVM, repoUtils)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    organization := "io.lullabyte",
    coverageMinimum := 0,
    coverageFailOnMinimum := true,
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion,
      "commit" -> git.gitHeadCommit.value.get
    ),
    buildInfoPackage := "rpm4s.cli",
    mainClass in assembly := Some("rpm4s.cli.Main"),
    fork in run := true,
    //javaOptions in run ++= Seq(
    //  //"-Xmx2000m",
    //  "-Xdebug",
    //  "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005"
    //),
    //fork in Test := true,
    //javaOptions in Test ++= Seq(
    //  "-Xdebug",
    //  "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=127.0.0.1:5005"
    //),
    assemblyOption in assembly := (assemblyOption in assembly).value
      .copy(prependShellScript = Some(defaultShellScript)),
    assemblyJarName in assembly := "rpm4s",
    assemblyOption in assembly :=
      (assemblyOption in assembly).value
        .copy(
          prependShellScript = Some(
            Seq("#!/usr/bin/env sh", """exec java -jar "$0" "$@"""" + "\n")
          )),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalatest,
      "org.scalacheck" %%% "scalacheck" % scalacheck,
      "org.typelevel" %%% "cats-core" % cats,
      "org.typelevel" %%% "cats-free" % cats,
      "co.fs2" %% "fs2-io" % fs2,
      "co.fs2" %%% "fs2-core" % fs2,
      //"co.fs2" %% "fs2-cats" % fs2Cats,
      "org.apache.commons" % "commons-compress" % "1.12",
      "org.tukaani" % "xz" % "1.5",
      "com.github.pathikrit" %% "better-files-akka" % "3.0.0",
      "com.github.scopt" %% "scopt" % "3.6.0",
      "org.http4s" %% "http4s-core" % http4s,
      "org.http4s" %% "http4s-dsl" % http4s,
      "org.http4s" %% "http4s-jawn" % http4s,
      "org.http4s" %% "http4s-blaze-server" % http4s,
      "org.http4s" %% "http4s-blaze-client" % http4s,
      "io.circe" %% "circe-core" % circe,
      "io.circe" %% "circe-generic" % circe,
      "io.circe" %% "circe-parser" % circe,
      "org.bouncycastle" % "bcprov-jdk15on" % "1.57",
      "org.bouncycastle" % "bcpg-jdk15on" % "1.57"
    )
  )

lazy val repoUtils = project.in(file("repo-utils"))
  .dependsOn(rpm4sJVM)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    organization := "io.lullabyte",
    coverageMinimum := 42,
    coverageFailOnMinimum := true,
    scalacOptions ++= Seq("-deprecation"),
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion
    ),
    buildInfoPackage := "rpm4s.repo",
    fork in run := true,
    //javaOptions in run ++= Seq(
    //  //"-Xmx2000m",
    //  "-Xdebug",
    //  "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005"
    //),
    //fork in Test := true,
    //javaOptions in Test ++= Seq(
    //  "-Xdebug",
    //  "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=127.0.0.1:5005"
    //),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalatest,
      "org.scalacheck" %%% "scalacheck" % scalacheck,
      "org.typelevel" %%% "cats-core" % cats,
      "org.typelevel" %%% "cats-free" % cats,
      "co.fs2" %% "fs2-io" % fs2,
      "co.fs2" %%% "fs2-core" % fs2,
      "org.apache.commons" % "commons-compress" % "1.12",
      "org.http4s" %% "http4s-core" % http4s,
      "io.circe" %% "circe-core" % circe,
      "io.circe" %% "circe-generic" % circe,
      "io.circe" %% "circe-parser" % circe
    )
  )
