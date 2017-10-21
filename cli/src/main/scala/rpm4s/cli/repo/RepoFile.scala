package rpm4s.cli.repo

import org.http4s.Uri

case class RepoFile(
    name: String,
    baseurl: Uri,
    enabled: Boolean = true,
    autorefresh: Boolean = true,
    gpgcheck: Boolean = false
)
object RepoFile {
  def toFile(repoFile: RepoFile): String = {
    s"""|[${repoFile.name}]
        |type=rpm-md
        |baseurl=${repoFile.baseurl}
        |enabled=${if (repoFile.enabled) "1" else "0"}
        |autorefresh=${if (repoFile.autorefresh) "1" else "0"}
        |gpgcheck=${if (repoFile.gpgcheck) "1" else "0"}
    """.stripMargin
  }
}
