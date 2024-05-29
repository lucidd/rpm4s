package rpm4s.cli

import java.nio.file.Path

import cats.syntax.all._
import com.monovore.decline._
import rpm4s.data.{Architecture, License}
import rpm4s.data._


case class RpmInfo(
 lead: Lead,
 name: Name,
 version: Version,
 release: Release,
 architecture: Architecture,
 vendor: Vendor,
 license: License,
 summery: Summary,
 description: Description,
 group: rpm4s.data.Group,
 compression: Compression,
 payloadFormat: PayloadFormat,
 packager: Option[Packager],
 epoch: Epoch = Epoch.ZERO,
 buildhost: Option[BuildHost] = None,
 buildtime: Option[BuildTime] = None,
)

case class GeneralOptions(verbosity: Int)

sealed trait CommandOptions extends Product with Serializable
case class InfoCommandOptions(rpmfile: Path) extends CommandOptions
case class CreateRepoCommandOptions(root: Path, revision: Option[Long], concurrent: Option[Int]) extends CommandOptions
case class ExtractCommandOptions(rpmfile: Path, out: Option[Path]) extends CommandOptions
case object VersionCommandOptions extends CommandOptions

case class AllOptions(general: GeneralOptions, command: CommandOptions)

object Main extends CommandApp(
  name = "rpm4s",
  header = "",
  version = rpm4s.cli.BuildInfo.version,
  main = {

    val info = Opts.subcommand("info", "prints some general info about an rpm (similar to rpm -qi)")(
       Opts.argument[Path]("rpmfile")
    ).map(InfoCommandOptions)

    val extract = Opts.subcommand("extract", "extract the files from an rpm.")(
      (Opts.argument[Path]("rpmfile"), Opts.argument[Path]("out").orNone).mapN(ExtractCommandOptions)
    )

    val build = Opts.subcommand("build", "build an rpm.")(Opts.unit).map { _ =>
      println("build")
    }

    val version = Opts.subcommand("version", "prints the rpm4s version.")(Opts.unit)
      .as(VersionCommandOptions)

    val createrepo = Opts.subcommand("createrepo", "creates a repomd repository from a folder or rpms.")(
      (
       Opts.option[Path]("dir", help = "directory containing rpms."),
       Opts.argument[Long]("rev").orNone,
       Opts.argument[Int]("concurrent").orNone
      ).mapN(CreateRepoCommandOptions)
    )

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
        case InfoCommandOptions(rpmfile) => commands.info(rpmfile)
        case CreateRepoCommandOptions(root, revision, concurrent) =>
          commands.createrepo(root, revision.getOrElse(0), concurrent.getOrElse(4))
        case ExtractCommandOptions(rpmfile, out) =>
          commands.extract(rpmfile, out)
        case VersionCommandOptions => commands.version()
      }
    }
  }
)
