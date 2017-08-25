import Dependencies._
import org.scalajs.jsenv.nodejs.NodeJSEnv


scalafmtVersion in ThisBuild := "1.0.0-RC2"

val DebugTest = config("dtest") extend Test

def scalacOptionsVersion(scalaVersion: String) = {
  Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
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
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
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
  .aggregate(rpm4sJS, rpm4sJVM)
  .settings(
    coverageEnabled := false,
    publishArtifact := false,
    publish := {},
    publishLocal := {}
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(rpm4sJVM)
  .enablePlugins(JmhPlugin)

lazy val rpm4s = crossProject
  .in(file("."))
  .configs(DebugTest)
  .settings(inConfig(DebugTest)(Defaults.testSettings): _*)
  .settings(
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
    scalaVersion := "2.12.3",
    crossScalaVersions := Seq("2.12.3"),
    name := "rpm4s",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.2",
      "org.scalatest" %%% "scalatest" % scalatest,
      "org.scalacheck" %%% "scalacheck" % scalacheck,
      "org.typelevel" %%% "cats-core" % cats,
      "co.fs2" %% "fs2-io" % fs2,
      "co.fs2" %%% "fs2-core" % fs2,
      "co.fs2" %% "fs2-cats" % fs2Cats,
      "org.scodec" %% "scodec-core" % "1.10.3",
      "org.scodec" %% "scodec-cats" % "0.2.0"
    ),
    scalacOptions ++= scalacOptionsVersion(scalaVersion.value),
    scalacOptions in (Compile, console) ~= (_.filterNot(
      Set(
        "-Ywarn-unused:imports",
        "-Xfatal-warnings"
      )))
  )
  .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoPackage := "rpm4s"
    )

lazy val rpm4sJVM = rpm4s.jvm
  .settings(
    libraryDependencies ++= Seq(
    )
  )

lazy val rpm4sJS = rpm4s.js
  .settings(
    parallelExecution := false,
    requiresDOM := false,
    jsEnv := new NodeJSEnv()
  )

