package rpm4s

import rpm4s.data.HeaderType

case class IndexEntry[+T <: RPMTag](tag: T, tpe: HeaderType, offset: Long, count: Long)
object IndexEntry {}
