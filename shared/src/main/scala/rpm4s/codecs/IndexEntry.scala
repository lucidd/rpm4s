package rpm4s.codecs

import rpm4s.data.HeaderType

case class IndexEntry[+T <: RPMTag](tag: T, tpe: HeaderType, offset: Long, count: Long)
object IndexEntry {}
