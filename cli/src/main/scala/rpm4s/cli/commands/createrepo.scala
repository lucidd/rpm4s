package rpm4s.cli.commands
import java.nio.channels.FileChannel
import java.nio.file.Path
import java.util.concurrent.Executors

import better.files.File
import cats.effect.{Effect, IO}
import fs2.Stream
import rpm4s.data.RpmPrimaryEntry
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector

import scala.concurrent.ExecutionContext

object createrepo {

  def apply(root: Path, revision: Long, concurrent: Int): Unit = {
    val primaryQuery = rpm4s.codecs.decoder[RpmPrimaryEntry]
    val executor = Executors.newFixedThreadPool(concurrent)
    implicit val EC = ExecutionContext.fromExecutor(executor)
    val rpms = rpm4s.utils.file.ls[IO](root)
      .filter(f => File(f).extension.contains(".rpm"))
      .map { path =>
        println(path)
        Stream.bracket(Effect[IO].delay {
          FileChannel.open(path)
        })(
          fc => {
            val bits = BitVector.fromMmap(fc)
            primaryQuery.decodeValue(bits) match {
              case Successful(rpm) =>
                Stream.emit((rpm, rpm4s.utils.hash.sha256(bits)))
              case Failure(err) =>
                Stream.empty
            }
          },
          fc =>
            Effect[IO].delay {
              fc.close()
            }
        )

      }
    rpm4s.repo.create(
      root,
      rpms.join(Int.MaxValue),
      (_, c) => s"${c.toSelfDescribingHex}.rpm",
      revision,
      None
    ).unsafeRunSync()
    executor.shutdown()
  }

}
