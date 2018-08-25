package rpm4s.data


sealed trait PayloadFormat
object PayloadFormat {
  case object CPIO extends PayloadFormat
  def fromString(value: String): Option[PayloadFormat] = value match {
    case "cpio" => Some(CPIO)
    case _ => None
  }
}
