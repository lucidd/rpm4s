package rpm4s.data

case class EVR(
    version: Version,
    release: Option[Release] = None,
    epoch: Option[Epoch] = None)
object EVR {
  //TODO: more validation for individual parts
  //TODO: Document format
  def parse(evr: String): Option[EVR] = {
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

    Version.parse(version).map { ver =>
      EVR(
        ver,
        release.map(Release(_)),
        epoch.map(x => Epoch(x.toInt))
      )
    }
  }
}
