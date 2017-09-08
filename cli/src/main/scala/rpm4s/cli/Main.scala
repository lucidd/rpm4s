package rpm4s.cli

import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.security.MessageDigest
import java.time.Instant
import java.util.concurrent.Executors

import better.files.File
import cats.effect.{IO, Sync}
import cats.implicits._
import fs2.{Chunk, Pipe, Sink, Stream}
import rpm4s.data.Checksum.Sha256
import rpm4s.data._
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import scala.util.Try

object Main {

    def main(args: Array[String]): Unit = {
      implicit val EC = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
      case class Config(dir: String)

      val parser = new scopt.OptionParser[Config]("rpm4s") {
        head("rpm4s", rpm4s.cli.BuildInfo.version)
        cmd("createrepo").action((_, c) => c)
          .text("create a repo from a directory of rpms")
          .children(
            arg[String]("path")
              .action { (path, c) =>
                c.copy(dir = path)
              }
              .text("path to a directory of rpms")
          )
      }

      val primaryQuery = rpm4s.codecs.decoder[RpmPrimaryEntry]

      parser.parse(args, Config("")) match {
        case Some(config) =>
          println(config)
          val root = Paths.get(config.dir)
          val rpms = rpm4s.utils.file.ls[IO](root)
            .filter(f => File(f).extension.contains(".rpm"))
            .map { path =>
              println(path)
              Stream.bracket(Sync[IO].delay {
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
                  Sync[IO].delay {
                    fc.close()
                  }
              )

            }
          rpm4s.repo.create(
            root,
            rpms.join(Int.MaxValue),
            (_, c) => s"${c.toSelfDescribingHex}.rpm",
            1234567,
            None
          ).unsafeRunSync()
        case None =>
        // arguments are bad, error message will have been displayed
      }

    }

}
