package rpm4s

import java.nio.charset.StandardCharsets
import java.nio.file._
import java.security.MessageDigest

import cats.effect.IO
import cats.implicits._
import fs2.{Chunk, Sink, Stream}
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import rpm4s.repo.utils.compress.gzip

package object repo {

  def create(
    reporoot: Path,
    rpms: Stream[IO, (RpmPrimaryEntry, Checksum)],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    revision: Long,
    count: Option[Long]
  ): IO[Unit] = {
    val primaryFile = reporoot.resolve("primary.xml.gz")
    val repomdFile = reporoot.resolve("repomd.xml")
    Files.deleteIfExists(primaryFile)
    Files.deleteIfExists(repomdFile)
    create(
      fs2.io.file.writeAll[IO](repomdFile),
      fs2.io.file.writeAll[IO](primaryFile),
      nameFn,
      rpms,
      revision,
      count
    )
  }

  def create(
    repomdSink: Sink[IO, Byte],
    primarySink: Sink[IO, Byte],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    rpms: Stream[IO, (RpmPrimaryEntry, Checksum)],
    revision: Long,
    count: Option[Long]
  ): IO[Unit] = {
    rpm4s.repo.utils.hash.checksumsBeforeAndAfter[IO](
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
        Stream.chunk(Chunk.bytes(repomdBytes)).covary[IO].to(repomdSink).run
    }
  }

}
