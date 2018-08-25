package rpm4s.data

sealed trait Compression
object Compression {
  case object LZMA extends Compression
  case object GZIP extends Compression
  case object XZ extends Compression
  def fromString(value: String): Option[Compression] = value match {
    case "lzma" => Some(LZMA)
    case "gzip" => Some(GZIP)
    case "xz" => Some(XZ)
    case _ => None
  }
}
