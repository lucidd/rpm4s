package rpm4s

import java.nio.charset.StandardCharsets
import java.nio.file._
import java.security.MessageDigest

import cats.effect.{Effect, Sync}
import cats.implicits._
import fs2.{Chunk, Sink, Stream}
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import rpm4s.repo.utils.compress.gzip
import cats.implicits._

package object repo {

  def create[F[_]: Effect](
    reporoot: Path,
    rpms: Stream[F, (RpmPrimaryEntry, Checksum)],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    revision: Long,
    count: Option[Long]
  ): F[Unit] = {
    val primaryFile = reporoot.resolve("primary.xml.gz")
    val repomdFile = reporoot.resolve("repomd.xml")
    Files.deleteIfExists(primaryFile)
    Files.deleteIfExists(repomdFile)
    create(
      fs2.io.file.writeAll(repomdFile),
      fs2.io.file.writeAll(primaryFile),
      nameFn,
      rpms,
      revision,
      count
    )
  }

  def create[F[_]: Effect](
    repomdSink: Sink[F, Byte],
    primarySink: Sink[F, Byte],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    rpms: Stream[F, (RpmPrimaryEntry, Checksum)],
    revision: Long,
    count: Option[Long]
  ): F[Unit] = {
    rpm4s.repo.utils.hash.checksumsBeforeAndAfter(
      repomd.xml.primary.create(count, rpms, nameFn).through(fs2.text.utf8Encode),
      gzip(),
      MessageDigest.getInstance("SHA-256"),
      Sha256.fromBytes,
      primarySink
    ).flatMap {
      case (openChecksum, openSize, gzChecksum, gzSize) =>
        val repomdBytes =
          repomd.create(gzSize, openSize, gzChecksum, openChecksum, revision)(
            new StringBuilder).toString.getBytes(StandardCharsets.UTF_8)
        Stream.chunk(Chunk.bytes(repomdBytes)).covary[F].to(repomdSink).compile.drain
    }
  }

}
