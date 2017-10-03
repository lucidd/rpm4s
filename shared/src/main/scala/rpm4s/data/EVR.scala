package rpm4s.data

import rpm4s.codecs.ConvertingError

case class EVR(
    version: Version,
    release: Option[Release] = None,
    epoch: Option[Epoch] = None) {
  def string: String = {
    val e = epoch.map(e => s"${e.value}:").getOrElse("")
    val r = release.map(r => s"-${r.value}").getOrElse("")
    s"$e${version.string}$r"
  }
}
object EVR {
  //TODO: more validation for individual parts
  //TODO: Document format
  def parse(evr: String): Either[ConvertingError, EVR] = {
    val relIdx = evr.indexOf("-")

    val (release, ev) = if (relIdx == -1) {
      (None, evr)
    } else {
      (Some(evr.substring(relIdx + 1)), evr.substring(0, relIdx))
    }

    val epochIdx = ev.indexOf(":")
    val (epoch, version) = if (epochIdx == -1) {
      (None, ev)
    } else {
      (Some(ev.substring(0, epochIdx)), ev.substring(epochIdx + 1))
    }
    for {
      ver <- Version.parse(version)
      rel <- release match {
        case Some(r) => Release.fromString(r).map(Some(_))
        case None => Right(None)
      }
      ep <- epoch match {
        case Some(e) =>  Epoch.fromString(e).map(Some(_))
        case None => Right(None)
      }
    } yield EVR(ver, rel, ep)
  }
}
