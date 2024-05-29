package rpm4s

import java.nio.charset.StandardCharsets
import java.nio.file._
import java.security.MessageDigest

import cats.effect.{Blocker, ContextShift, Effect}
import cats.syntax.all._
import fs2.{Chunk, Pipe, Stream}
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import rpm4s.repo.utils.compress.gzip
import rpm4s.repo.data.updateinfo.UpdateF.Update
import cats.syntax.all._


package object repo {

  def create[F[_]: Effect: ContextShift](
    reporoot: Path,
    rpms: Stream[F, (RpmPrimaryEntry, Checksum)],
    updates: Stream[F, Update],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    revision: Long,
    count: Option[Long],
    blocker: Blocker
  ): F[Unit] = {
    val primaryFile = reporoot.resolve("primary.xml.gz")
    val updateinfoFile = reporoot.resolve("updateinfo.xml.gz")
    val repomdFile = reporoot.resolve("repomd.xml")
    Files.deleteIfExists(primaryFile)
    Files.deleteIfExists(repomdFile)
    create(
      fs2.io.file.writeAll(repomdFile, blocker),
      fs2.io.file.writeAll(primaryFile, blocker),
      fs2.io.file.writeAll(updateinfoFile, blocker),
      nameFn,
      rpms,
      updates,
      revision,
      count
    )
  }

  def create[F[_]: Effect](
    repomdSink: Pipe[F, Byte, Unit],
    primarySink: Pipe[F, Byte, Unit],
    updateinfoSink: Pipe[F, Byte, Unit],
    nameFn: (RpmPrimaryEntry, Checksum) => String,
    rpms: Stream[F, (RpmPrimaryEntry, Checksum)],
    updates: Stream[F, Update],
    revision: Long,
    count: Option[Long]
  ): F[Unit] = {

    val updateinfo = rpm4s.repo.utils.hash.checksumsBeforeAndAfter(
      repomd.xml.updateinfo.create(updates).through(fs2.text.utf8Encode),
      gzip(),
      MessageDigest.getInstance("SHA-256"),
      Sha256.fromBytes,
      updateinfoSink
    )
    val primary = rpm4s.repo.utils.hash.checksumsBeforeAndAfter(
      repomd.xml.primary.create(count, rpms, nameFn).through(fs2.text.utf8Encode),
      gzip(),
      MessageDigest.getInstance("SHA-256"),
      Sha256.fromBytes,
      primarySink
    )
    (primary, updateinfo).tupled.flatMap {
      case ((primOpenChecksum, primOpenSize, primGzChecksum, primGzSize), (upOpenChecksum, upOpenSize, upGzChecksum, upGzSize)) =>
        val repomdBytes =
          repomd.create(
            primGzSize, primOpenSize, primGzChecksum, primOpenChecksum,
            upGzSize, upOpenSize, upGzChecksum, upOpenChecksum,
            revision)(new StringBuilder)
            .toString
            .getBytes(StandardCharsets.UTF_8)
        Stream.chunk(Chunk.bytes(repomdBytes)).covary[F].through(repomdSink).compile.drain
    }
  }

}
