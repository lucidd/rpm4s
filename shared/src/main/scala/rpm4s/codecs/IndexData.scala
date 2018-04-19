package rpm4s.codecs

import rpm4s.data.HeaderType
import scodec.bits.ByteVector

sealed trait IndexData extends Product with Serializable {
  def count: Long
  def headerType: HeaderType
}
object IndexData {
  /* TODO: string encoding seems to be effected by the
    encoding header which means we may need to defer decoding them fully
   */
  case class StringData(value: String) extends IndexData {
    override def count: Long = 1
    override def headerType: HeaderType = HeaderType.String
  }
  case class BinaryData(value: ByteVector) extends IndexData {
    override def count: Long = value.size
    override def headerType: HeaderType = HeaderType.Bin
  }
  case class Int8Data(values: Vector[Byte]) extends IndexData {
    override def count: Long = values.size.toLong
    override def headerType: HeaderType = HeaderType.Int8
  }
  case class Int16Data(values: Vector[Short]) extends IndexData {
    override def count: Long = values.size.toLong
    override def headerType: HeaderType = HeaderType.Int16
  }
  case class Int32Data(values: Vector[Int]) extends IndexData {
    override def count: Long = values.size.toLong
    override def headerType: HeaderType = HeaderType.Int32
  }
  case class Int64Data(values: Vector[Long]) extends IndexData {
    override def count: Long = values.size.toLong
    override def headerType: HeaderType = HeaderType.Int64
  }
  case class StringArrayData(values: Vector[String]) extends IndexData {
    override def count: Long = values.size.toLong
    override def headerType: HeaderType = HeaderType.StringArray
  }
  case class I18NStringArrayData(values: Vector[String]) extends IndexData {
    override def count: Long = values.size.toLong
    override def headerType: HeaderType = HeaderType.I18NString
  }
}
