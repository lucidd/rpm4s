package rpm4s.repo.data

import rpm4s.repo.data.Data.{Primary, UpdateInfo}

case class Repomd(
  revision: Long,
  primary: Option[Primary],
  updateinfo: Option[UpdateInfo]
)

object Repomd {

  case class RepoMdBuilder(
      revision: Option[Long],
      primary: Option[Primary],
      updateinfo: Option[UpdateInfo]
  )
  object RepoMdBuilder {
    val empty: RepoMdBuilder = apply(None, None, None)
    def build(builder: RepoMdBuilder): Option[Repomd] = {
      builder.revision.map { rev =>
        Repomd(rev, builder.primary, builder.updateinfo)
      }
    }
  }
}

