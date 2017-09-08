package rpm4s.repo.yast2

import org.http4s.Uri

case class Content(
    version: String,
    datadir: Uri,
    descrdir: Uri,
)

object Content {
  def fromLines(lines: List[String]): Option[Content] = {
    val map = lines
      .filter(_.nonEmpty)
      .map { line =>
        val parts = line.split("\\s+", 2)
        parts(0) -> parts(1)
      }
      .toMap
    Some(
      Content(
        version = map("CONTENTSTYLE"),
        datadir = Uri.unsafeFromString(map("DATADIR")),
        descrdir = Uri.unsafeFromString(map("DESCRDIR")),
      )
    )
  }
}
