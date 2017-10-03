package rpm4s.data

trait PkgRef {
  /**
    * Note: name does not seem to always be a valid rpm name see kernel-default provides
    * firmware(4.11.8-1-default/3com/typhoon.bin)
    * @return
    */
  def name: String
  def evr: Option[EVR]
  def flags: SenseFlags
  def rpmLib: Option[PkgRef.RpmLib] = {
    val prefix = "rpmlib("
    val (p, r) = name.splitAt(prefix.length)
    if (p == prefix && r.endsWith(")")) {
      val v = r.take(r.length - 1)
      PkgRef.RpmLib.fromString(v)
    } else None
  }
}
object PkgRef {
  sealed trait RpmLib extends Product with Serializable
  object RpmLib {
    case object CompressedFileNames extends RpmLib
    case object PayloadFilesHavePrefix extends RpmLib
    case object PayloadIsLzma extends RpmLib
    case object TildeInVersions extends RpmLib
    def fromString(value: String): Option[RpmLib] = value match {
      case "CompressedFileNames" => Some(CompressedFileNames)
      case "PayloadFilesHavePrefix" => Some(PayloadFilesHavePrefix)
      case "PayloadIsLzma" => Some(PayloadIsLzma)
      case "TildeInVersions" => Some(TildeInVersions)
      case _ => None
    }
  }
}
