package rpm4s.data

sealed trait PkgRef extends Product with Serializable {
  def nameString: String
  def versionString: Option[String]
  def flags: SenseFlags
  def rpmLib: Option[PkgRef.RpmLib] = {
    val prefix = "rpmlib("
    val (p, r) = nameString.splitAt(prefix.length)
    if (p == prefix && r.endsWith(")")) {
      val v = r.take(r.length - 1)
      PkgRef.RpmLib.fromString(v)
    } else None
  }
}
case class RpmRef(name: Name, evr: Option[EVR], flags: SenseFlags) extends PkgRef {
  override def nameString: String = name.value
  override def versionString: Option[String] = evr.map(_.string)
}
case class VirtualRef(name: String, version: Option[String], flags: SenseFlags) extends PkgRef {
  override def nameString: String = name
  override def versionString: Option[String] = version
}
object PkgRef {
  sealed trait RpmLib extends Product with Serializable
  object RpmLib {
    //TODO: find the remaining possible values
    case object CompressedFileNames extends RpmLib
    case object PayloadFilesHavePrefix extends RpmLib
    case object PayloadIsLzma extends RpmLib
    case object TildeInVersions extends RpmLib
    case object PartialHardlinkSets extends RpmLib
    case object FileDigests extends RpmLib
    case object PayloadIsXz extends RpmLib


    def fromString(value: String): Option[RpmLib] = value match {
      case "CompressedFileNames" => Some(CompressedFileNames)
      case "PayloadFilesHavePrefix" => Some(PayloadFilesHavePrefix)
      case "PartialHardlinkSets" => Some(PartialHardlinkSets)
      case "PayloadIsLzma" => Some(PayloadIsLzma)
      case "TildeInVersions" => Some(TildeInVersions)
      case "FileDigests" =>  Some(FileDigests)
      case "PayloadIsXz" => Some(PayloadIsXz)
      case _ => None
    }
  }
}
