package rpm4s.cli.commands

object version {

  def apply(): Unit = {
    val output =
      s"""|Version: ${rpm4s.cli.BuildInfo.version}
          |Commit: ${rpm4s.cli.BuildInfo.commit}""".stripMargin
    println(output)
  }

}
