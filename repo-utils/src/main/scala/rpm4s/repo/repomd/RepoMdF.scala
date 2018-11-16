package rpm4s.repo.repomd

import cats.Id
import cats.implicits._
import rpm4s.repo.repomd.Data.{Primary, UpdateInfo}


case class RepoMdF[F[_]](
  revision: F[Long],
  primary: F[Primary],
  updateinfo: F[UpdateInfo]
)

object RepoMdF {
  type RepoMd = RepoMdF[cats.Id]
  type RepoMdBuilder = RepoMdF[Option]
  object RepoMdBuilder {
    val empty: RepoMdBuilder = apply()
    def apply(
      revision: Option[Long] = None,
      primary: Option[Primary] = None,
      updateinfo: Option[UpdateInfo] = None
    ): RepoMdBuilder = RepoMdF(revision, primary, updateinfo)

    def build(builder: RepoMdBuilder): Option[RepoMd] = {
      (builder.revision, builder.primary, builder.updateinfo).mapN(RepoMdF[Id])
    }
  }
}

