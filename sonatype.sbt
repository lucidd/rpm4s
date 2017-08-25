sonatypeProfileName := "io.lullabyte"
publishMavenStyle := true
licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
homepage := Some(url("https://github.com/lucidd/rpm4s"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/lucidd/rpm4s"),
    "scm:git@github.com:lucidd/rpm4s.git"
  )
)
developers := List(
  Developer(
    id="lucidd",
    name="Kevin Walter",
    email="kevin.walter.private@gmail.com",
    url=url("https://lullabyte.io")
  )
)
