package rpm4s.repo.data

case class Bytes(value: Long) extends AnyVal

object Bytes {
  def kb(value: Long): Bytes = Bytes(value * 1024)
  def mb(value: Long): Bytes = Bytes(value * 1024 * 1024)
  def fromString(value: String): Option[Bytes] =
    value.split(" ", 2) match {
      case Array(x, "KB") => Some(kb(x.toLong))
      case Array(x, "MB") => Some(mb(x.toLong))
      case _              => None
    }
}
