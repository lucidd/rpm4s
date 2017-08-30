package rpm4s.codecs

sealed trait Error
case class MissingHeader(tag: RPMTag) extends Error
case class ConvertingError(msg: String) extends Error
