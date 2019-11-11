package rpm4s.cli.commands
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.util.concurrent.Executors

import better.files.File
import cats.effect.{Blocker, Effect, IO}
import fs2.Stream
import rpm4s.data.RpmPrimaryEntry
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector

import scala.concurrent.ExecutionContext
import rpm4s.data.Checksum

object createrepo {

  def apply(root: Path, revision: Long, concurrent: Int): Unit = {
    val primaryQuery = rpm4s.codecs.decoder[RpmPrimaryEntry]
    val executor = Executors.newFixedThreadPool(concurrent)
    implicit val EC = ExecutionContext.fromExecutor(executor)
    implicit val CS = IO.contextShift(EC)
    val rpms = rpm4s.utils.file.ls[IO](root)
      .filter(f => File(f).extension.contains(".rpm"))
      .map { path =>
        println(path)
        Stream.bracket(Effect[IO].delay {
          FileChannel.open(path)
        })(
          fc =>
            Effect[IO].delay {
              fc.close()
            }
        ).flatMap {
          fc =>
            val bits = BitVector.fromMmap(fc)
            primaryQuery.decodeValue(bits) match {
              case Successful(rpm) =>
                Stream.emit((rpm, rpm4s.utils.hash.sha256(bits)))
              case Failure(err) =>
                Stream.empty
            }
        }

      }
    rpm4s.repo.create[IO](
      root,
      rpms.parJoin(Int.MaxValue),
      Stream.empty,
      (_: RpmPrimaryEntry, c: Checksum) => s"${c.toSelfDescribingHex}.rpm",
      revision,
      None,
      Blocker.liftExecutionContext(EC)
    ).unsafeRunSync()
    executor.shutdown()
  }

}
