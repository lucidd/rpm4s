package rpm4s.data

case class HeaderRange(start: Long, end: Long)
object HeaderRange {
  def startSize(start: Long, size: Long): HeaderRange =
    HeaderRange(start, start + size)
}
