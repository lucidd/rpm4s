package rpm4s.codecs

/**
  *
  * @param version
  * @param indexSize number of index entries
  * @param hsize size of the data referenced by the index (in bytes)
  * @param index index entries
  * @tparam T
  */
case class Header[+T <: RPMTag](
  version: Int,
  indexSize: Int,
  hsize: Long,
  index: List[IndexEntry[T]]
)
object Header {}
