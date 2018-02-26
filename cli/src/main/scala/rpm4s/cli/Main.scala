package rpm4s.cli

import java.nio.channels.FileChannel
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import java.time.format.DateTimeFormatter

import cats.syntax._
import cats.implicits._
import com.monovore.decline._
import rpm4s.data.{Architecture, License}
import scodec.Attempt.{Failure, Successful}
import rpm4s.data._
import scodec.bits.BitVector
import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import java.io.BufferedInputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.concurrent.Executors

import better.files.File
import fs2._
import cats.effect.{Effect, IO}
import org.apache.commons.compress.archivers.cpio.{CpioArchiveEntry, CpioArchiveInputStream}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

case class RpmInfo(
 name: Name,
 version: Version,
 release: Release,
 architecture: Architecture,
 vendor: Vendor,
 license: License,
 summery: Summery,
 description: Description,
 group: rpm4s.data.Group,
 compression: Compression,
 packager: Packager,
 epoch: Option[Epoch] = None,
 buildhost: Option[BuildHost] = None,
 buildtime: Option[BuildTime] = None,
)

case class GeneralOptions(verbosity: Int)

sealed trait CommandOptions extends Product with Serializable
case class InfoCommandOptions(rpmfile: Path) extends CommandOptions
case class CreateRepoCommandOptions(root: Path) extends CommandOptions
case class ExtractCommandOptions(rpmfile: Path) extends CommandOptions
case object VersionCommandOptions extends CommandOptions

case class AllOptions(general: GeneralOptions, command: CommandOptions)


object Test {

  def createrepo(root: Path): Unit = {
    val primaryQuery = rpm4s.codecs.decoder[RpmPrimaryEntry]
    val executor = Executors.newFixedThreadPool(4)
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
      1234567,
      None
    ).unsafeRunSync()
    executor.shutdown()
  }

  def version(): Unit = {
    val output =
      s"""|Version: ${rpm4s.cli.BuildInfo.version}
          |Commit: ${rpm4s.cli.BuildInfo.commit}""".stripMargin
    println(output)
  }

  def extract(path: Path): Unit = {
    val bits = BitVector.fromMmap(FileChannel.open(path))
    rpm4s.decode[Payload](bits) match {
      case Successful(payload) =>
        val out = Paths.get("payload.cpio.xz")
        payload.bitVector.bytes.copyToStream(Files.newOutputStream(out))

        val fin = Files.newInputStream(out)
        val in = new BufferedInputStream(fin)
        val lzmaIn = new LZMACompressorInputStream(in)
        val cpioIn = new CpioArchiveInputStream(lzmaIn)

        val base = Paths.get("./payload")

        @tailrec
        def unpackCpio(in: CpioArchiveInputStream, hardlinks: List[CpioArchiveEntry]): Unit = {
          val entry = in.getNextCPIOEntry
          if (entry != null) {
            val path = base.resolve(entry.getName)
            println(path.normalize().toAbsolutePath.toString)
            if (entry.isDirectory) {
              Files.createDirectories(path)
              unpackCpio(in, hardlinks)
            } else if (entry.isRegularFile) {
              if (entry.getSize == 0) {
                unpackCpio(in, hardlinks :+ entry)
              } else {
                path.toFile.getParentFile.mkdirs()
                path.toFile.getParentFile.mkdirs()
                val buffer = new Array[Byte](1024 * 16)
                copyCpioEntry(in, Files.newOutputStream(path), buffer, entry.getSize.toInt)
                path.toFile.setLastModified(entry.getLastModifiedDate.getTime)

                hardlinks.foreach { hardlink =>
                  val linkPath = base.resolve(hardlink.getName)
                  Files.createLink(linkPath, path)
                }

                unpackCpio(in, List.empty)
              }
            }
          }
        }

        def copyCpioEntry(in: CpioArchiveInputStream, out: OutputStream, buffer: Array[Byte], remaining: Int): Unit = {
          if (remaining > 0) {
            val bytes = in.read(buffer, 0, remaining.min(buffer.length))
            out.write(buffer, 0, bytes)
            copyCpioEntry(in, out, buffer, remaining - bytes)
          }
        }

        unpackCpio(cpioIn, List.empty)

      case Failure(cause) =>
        println(s"Error parsing rpm: $cause")
        System.exit(-1)
    }
  }

  def info(path: Path): Unit = {
    try {
      val bits = BitVector.fromMmap(FileChannel.open(path))
      rpm4s.decode[RpmInfo](bits) match {
        case Successful(info) =>

          val output =
            s"""|Name        : ${info.name.value}
                |Version     : ${info.version.string}
                |Release     : ${info.release.value}
                |Architecture: ${Architecture.toRpmString(info.architecture)}
                |Group       : ${info.group.locales}
                |License     : ${License.format(info.license)}
                |Compression : ${info.compression}
                |Build Date  : ${info.buildtime.fold("(none)")(x => DateTimeFormatter.ISO_INSTANT.format(x.time))}
                |Build Host  : ${info.buildhost.fold("(none)")(_.value)}
                |Packager    : ${info.packager.value}
                |Vendor      : ${info.vendor.value}
                |Summary     : ${info.summery.locales}
                |Description : ${info.description.locales}""".stripMargin
          println(output)

        case Failure(cause) =>
          println(s"Error parsing rpm: $cause")
          System.exit(-1)
      }
    } catch {
      case ex: NoSuchFileException =>
        println(s"file ${path.toAbsolutePath.toString} does not exist.")
        System.exit(-1)
    }
  }
}

object Main extends CommandApp(
  name = "rpm4s",
  header = "",
  version = rpm4s.cli.BuildInfo.version,
  main = {

    val info = Opts.subcommand("info", "prints some general info about an rpm (similar to rpm -qi)")(
       Opts.argument[Path]("rpmfile")
    ).map(InfoCommandOptions)

    val extract = Opts.subcommand("extract", "extract the files from an rpm.")(
      Opts.argument[Path]("rpmfile")
    ).map(ExtractCommandOptions)

    val build = Opts.subcommand("build", "build an rpm.")(Opts.unit).map { _ =>
      println("build")
    }

    val version = Opts.subcommand("version", "prints the rpm4s version.")(Opts.unit)
      .map(_ => VersionCommandOptions)

    val createrepo = Opts.subcommand("createrepo", "creates a repomd repository from a folder or rpms.")(
      Opts.option[Path]("dir", help = "directory containing rpms.")
    ).map(CreateRepoCommandOptions)

    val verbose = Opts.flags("verbose", "set the output verbosity", "v")
      .withDefault(0)
      .map { verbosity =>
        GeneralOptions(verbosity)
      }
    val subcommands = info.orElse(version)
        .orElse(createrepo)
        .orElse(extract)

    (verbose, subcommands).mapN { (general, command) =>
      command match {
        case InfoCommandOptions(rpmfile) => Test.info(rpmfile)
        case CreateRepoCommandOptions(root) => Test.createrepo(root)
        case ExtractCommandOptions(rpmfile) => Test.extract(rpmfile)
        case VersionCommandOptions => Test.version()
      }
    }
  }
)
