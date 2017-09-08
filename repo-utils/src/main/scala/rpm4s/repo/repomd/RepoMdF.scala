package rpm4s.repo.repomd

import cats.Id
import cats.implicits._
import rpm4s.repo.repomd.Data.Primary


case class RepoMdF[F[_]](
  revision: F[Long],
  primary: F[Primary]
)

object RepoMdF {
  type RepoMd = RepoMdF[cats.Id]
  type RepoMdBuilder = RepoMdF[Option]
  object RepoMdBuilder {
    val empty: RepoMdBuilder = apply()
    def apply(
      revision: Option[Long] = None,
      primary: Option[Primary] = None
    ): RepoMdBuilder = RepoMdF(revision, primary)

    def build(rmdDataBuilder: RepoMdBuilder): Option[RepoMd] = {
      (rmdDataBuilder.revision, rmdDataBuilder.primary).mapN(RepoMdF[Id])
    }
  }
}

