package rpm4s.cli.commands
import java.nio.channels.FileChannel
import java.nio.file.{NoSuchFileException, Path}
import java.time.format.DateTimeFormatter

import rpm4s.cli.RpmInfo
import rpm4s.data.{Architecture, License}
import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector

object info {

  def apply(path: Path): Unit = {
    try {
      val bits = BitVector.fromMmap(FileChannel.open(path))
      rpm4s.decode[RpmInfo](bits) match {
        case Successful(info) =>

          val output =
            s"""|Name           : ${info.name.value}
                |Version        : ${info.version.string}
                |Type           : ${info.lead.tpe}
                |Release        : ${info.release.value}
                |Architecture   : ${Architecture.toRpmString(info.architecture)}
                |Group          : ${info.group.locales}
                |License        : ${License.format(info.license)}
                |Compression    : ${info.compression}
                |Payload Format : ${info.payloadFormat}
                |Build Date     : ${info.buildtime.fold("(none)")(x => DateTimeFormatter.ISO_INSTANT.format(x.time))}
                |Build Host     : ${info.buildhost.fold("(none)")(_.value)}
                |Packager       : ${info.packager.fold("(none)")(_.value)}
                |Vendor         : ${info.vendor.value}
                |Summary        : ${info.summery.locales}
                |Description    : ${info.description.locales}""".stripMargin
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
