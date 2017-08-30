package rpm4s.codecs

import scodec.bits.ByteVector

sealed trait IndexData extends Product with Serializable {}
object IndexData {
  /* TODO: string encoding seems to be effected by the
    encoding header which means we may need to defer decoding them fully
   */
  case class StringData(value: String) extends IndexData
  case class BinaryData(value: ByteVector) extends IndexData
  case class Int8Data(values: Vector[Byte]) extends IndexData
  case class Int16Data(values: Vector[Short]) extends IndexData
  case class Int32Data(values: Vector[Int]) extends IndexData
  case class Int64Data(values: Vector[Long]) extends IndexData
  case class StringArrayData(values: Vector[String]) extends IndexData
  case class I18NStringArrayData(values: Vector[String]) extends IndexData
}
