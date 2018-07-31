package rpm4s.data

import rpm4s.codecs.ConvertingError
import cats.implicits._

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

  implicit val ordering: Ordering[EVR] = new Ordering[EVR] {
    override def compare(x: EVR, y: EVR): Int = {
      val epochCmp = Ordering[Option[Epoch]].compare(x.epoch, y.epoch)
      if (epochCmp != 0) {
        epochCmp
      } else {
        val versionCmp = Ordering[Version].compare(x.version, y.version)
        if (versionCmp != 0) versionCmp
        else {
          val releaseCmp = Ordering[Option[Release]].compare(x.release, y.release)
          releaseCmp
        }
      }
    }
  }


  //TODO: more validation for individual parts
  //TODO: Document format
  def parse(evr: String): Either[ConvertingError, EVR] = {
    // we split at the last - which is consistent with how rpm does it
    val relIdx = evr.lastIndexOf("-")

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
      ver <- Version.fromString(version)
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
