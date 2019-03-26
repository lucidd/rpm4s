package rpm4s

import java.nio.charset.StandardCharsets
import java.nio.file._
import java.security.MessageDigest

import cats.effect.{ContextShift, Effect}
import cats.implicits._
import fs2.{Chunk, Pipe, Stream}
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import rpm4s.repo.utils.compress.gzip
import cats.implicits._

import scala.concurrent.ExecutionContext

package object repo {

  def create[F[_]: Effect: ContextShift](
    reporoot: Path,
    rpms: Stream[F, (RpmPrimaryEntry, Checksum)],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    revision: Long,
    count: Option[Long],
    blockingEC: ExecutionContext
  ): F[Unit] = {
    val primaryFile = reporoot.resolve("primary.xml.gz")
    val repomdFile = reporoot.resolve("repomd.xml")
    Files.deleteIfExists(primaryFile)
    Files.deleteIfExists(repomdFile)
    create(
      fs2.io.file.writeAll(repomdFile, blockingEC),
      fs2.io.file.writeAll(primaryFile, blockingEC),
      nameFn,
      rpms,
      revision,
      count
    )
  }

  def create[F[_]: Effect](
    repomdSink: Pipe[F, Byte, Unit],
    primarySink: Pipe[F, Byte, Unit],
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
        Stream.chunk(Chunk.bytes(repomdBytes)).covary[F].through(repomdSink).compile.drain
    }
  }

}
