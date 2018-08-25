import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.Instant

import rpm4s.{Build, build}
import rpm4s.build.RpmBlueprint
import rpm4s.data.Dependency.Requires
import rpm4s.data.Stat.FileType
import rpm4s.data._
import sbt.Keys._
import sbt._
import sbtassembly.{Assembly, AssemblyKeys, AssemblyPlugin}
import scodec.bits.{BitVector, ByteVector}

object Rpm4sPlugin extends AutoPlugin {

  override def requires: Plugins = AssemblyPlugin

  object autoImport {

    val rpm4sDescription = SettingKey[Map[String, String]]("description")
    val rpm4sSummery = SettingKey[Map[String, String]]("summery")
    val rpm4sGroup = SettingKey[Map[String, String]]("group")
    val rpm4sBuild = TaskKey[File]("rpm4sBuild")

    lazy val baseSettings: Seq[Def.Setting[_]] = Seq(
      rpm4sDescription := Map(
        "C" -> description.value
      ),
      rpm4sSummery := Map(
        "C" -> description.value
      ),
      rpm4sGroup := Map(
        "C" -> "Development"
      ),
      rpm4sBuild := {
        val n = name.value
        val ver = version.value.replace('-', '.')
        val file = sbtassembly.AssemblyKeys.assemblyOutputPath.in(AssemblyKeys.assembly).value
        val f = file.getAbsoluteFile.toPath
        val fc = FileChannel.open(f)
        val bytes = BitVector.fromMmap(fc).bytes

        val testBlueprint: RpmBlueprint = RpmBlueprint(
          Name(n).toOption.get,
          Version.parse(ver).toOption.get,
          Release.fromString("0").toOption.get,
          None,
          License.MIT,
          Architecture.NoArch,
          Description(rpm4sDescription.value),
          Summery(rpm4sSummery.value),
          Group(rpm4sGroup.value),
          requires = List(
            Requires(
              RpmRef(Name("jre").toOption.get, Some(EVR.apply(Version.parse("1.8.0").toOption.get)), SenseFlags.GreaterEqual)
            )
          ),
          files = List(
            build.FileInfo(
              FileEntry("/usr/bin/rpm4s", Stat(owner = Stat.Perm.RWX, group = Stat.Perm.RX, other = Stat.Perm.RX,
                fileType = FileType.RegularFile), FileFlags.None),
              bytes,
              Instant.now(),
              "root",
              "root"
            )
          )
        )

        val rpmFile = Build.buildRpm(testBlueprint)
        val bits = rpm4s.codecs.rpmFileCodec.encode(rpmFile).require
        val out = Paths.get(s"$n-$ver.rpm")
        rpm4s.codecs.rpmFileCodec.decode(bits).require
        bits.bytes.copyToStream(Files.newOutputStream(out))
        file
      }
    )

  }


  import autoImport._

  override val projectSettings = baseSettings

}
