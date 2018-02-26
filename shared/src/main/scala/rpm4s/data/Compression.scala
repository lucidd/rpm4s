package rpm4s.data

sealed trait Compression
object Compression {
  case object LZMA extends Compression
  def fromString(value: String): Option[Compression] = value match {
    case "lzma" => Some(LZMA)
    case _ => None
  }
}
